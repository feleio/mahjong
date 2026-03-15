"""
aws_train.py — Launch a training job on AWS EC2 (GPU) or SageMaker.

Two modes are supported:

  1. EC2 launch  (``--mode ec2``)
       Spins up a p3.2xlarge (or any instance you choose), rsyncs your
       project, and runs ``self_play.py`` or ``train.py`` remotely.
       Requires: AWS CLI configured, an EC2 key-pair, a security group
       that allows SSH.

  2. SageMaker   (``--mode sagemaker``)
       Packages the code as a SageMaker PyTorch training job.
       Uses a managed GPU instance (ml.p3.2xlarge by default).
       Outputs (checkpoints) are automatically written to S3.
       Requires: AWS CLI + boto3, an IAM role with SageMaker + S3 access.

Both modes stream logs back to your terminal.

Usage — EC2
──────────────────────────────────────────────────────────────
    python rl/aws_train.py --mode ec2 \\
        --key-name my-keypair \\
        --security-group sg-0abc123 \\
        --instance-type p3.2xlarge \\
        --region us-east-1 \\
        --total-games 200000

Usage — SageMaker
──────────────────────────────────────────────────────────────
    python rl/aws_train.py --mode sagemaker \\
        --role-arn arn:aws:iam::123456789:role/SageMakerRole \\
        --s3-bucket my-mahjong-bucket \\
        --instance-type ml.p3.2xlarge \\
        --total-games 200000

Prerequisites
──────────────────────────────────────────────────────────────
    pip install boto3 sagemaker
    aws configure   # set your credentials and region
"""

import argparse
import os
import subprocess
import sys
import textwrap
import time
from pathlib import Path


# ── Shared helpers ─────────────────────────────────────────────────────────────

PROJECT_ROOT = Path(__file__).parent.parent  # mahjong/


def _run(cmd: str, **kwargs) -> None:
    print(f"$ {cmd}")
    subprocess.run(cmd, shell=True, check=True, **kwargs)


# ── EC2 mode ───────────────────────────────────────────────────────────────────

EC2_SETUP_SCRIPT = textwrap.dedent("""\
    #!/bin/bash
    set -e
    # Install Java 11
    sudo apt-get update -qq
    sudo apt-get install -y openjdk-11-jdk-headless python3-pip unzip curl

    # Install sbt
    curl -fsSL https://scala.jfrog.io/artifactory/debian/sbt.deb -o /tmp/sbt.deb
    sudo dpkg -i /tmp/sbt.deb || true
    sudo apt-get install -f -y

    # Python deps
    cd /home/ubuntu/mahjong
    pip3 install torch --index-url https://download.pytorch.org/whl/cu118
    pip3 install numpy

    # Build fat-jar
    cd /home/ubuntu/mahjong
    sbt assembly

    echo "Setup complete."
""")

TRAIN_SCRIPT_TEMPLATE = textwrap.dedent("""\
    #!/bin/bash
    set -e
    cd /home/ubuntu/mahjong
    JAR=$(find . -name '*.jar' | grep assembly | head -1)
    python3 rl/self_play.py \\
        --jar "$JAR" \\
        --total-games {total_games} \\
        --device cuda \\
        --save-dir /home/ubuntu/mahjong/rl/checkpoints \\
        2>&1 | tee /home/ubuntu/train.log
""")


def launch_ec2(args: argparse.Namespace) -> None:
    import boto3  # type: ignore

    ec2 = boto3.resource("ec2", region_name=args.region)

    # Deep Learning AMI (Ubuntu 20.04, us-east-1 — update per region as needed)
    ami_map = {
        "us-east-1": "ami-0c7217cdde317cfec",   # Ubuntu 22.04 LTS
        "us-west-2": "ami-0efcece6bed30fd98",
        "eu-west-1": "ami-0905a3c97561e0b69",
    }
    ami_id = args.ami or ami_map.get(args.region, ami_map["us-east-1"])

    print(f"Launching {args.instance_type} in {args.region} …")
    instances = ec2.create_instances(
        ImageId=ami_id,
        InstanceType=args.instance_type,
        KeyName=args.key_name,
        SecurityGroupIds=[args.security_group],
        MinCount=1,
        MaxCount=1,
        TagSpecifications=[{
            "ResourceType": "instance",
            "Tags": [{"Key": "Name", "Value": "mahjong-rl-training"}],
        }],
        BlockDeviceMappings=[{
            "DeviceName": "/dev/sda1",
            "Ebs": {"VolumeSize": 30, "DeleteOnTermination": True},
        }],
    )

    inst = instances[0]
    print(f"Instance {inst.id} starting …")
    inst.wait_until_running()
    inst.reload()
    ip = inst.public_ip_address
    print(f"Instance running at {ip}")

    ssh = f"ssh -o StrictHostKeyChecking=no -i {args.key_name}.pem ubuntu@{ip}"

    # Wait for SSH
    print("Waiting for SSH …")
    for _ in range(30):
        result = subprocess.run(f"{ssh} echo ok", shell=True,
                                capture_output=True, text=True)
        if result.returncode == 0:
            break
        time.sleep(10)

    # Rsync project
    print("Uploading project …")
    _run(
        f"rsync -az --exclude '.git' --exclude 'target' "
        f"-e 'ssh -o StrictHostKeyChecking=no -i {args.key_name}.pem' "
        f"{PROJECT_ROOT}/ ubuntu@{ip}:/home/ubuntu/mahjong/"
    )

    # Setup
    setup_path = "/tmp/ec2_setup.sh"
    Path(setup_path).write_text(EC2_SETUP_SCRIPT)
    _run(f"scp -i {args.key_name}.pem {setup_path} ubuntu@{ip}:/tmp/setup.sh")
    _run(f"{ssh} 'bash /tmp/setup.sh'")

    # Train (run in background, tail log)
    train_script = TRAIN_SCRIPT_TEMPLATE.format(total_games=args.total_games)
    train_path = "/tmp/ec2_train.sh"
    Path(train_path).write_text(train_script)
    _run(f"scp -i {args.key_name}.pem {train_path} ubuntu@{ip}:/tmp/train.sh")
    print("\nStarting training (streaming logs) …  Ctrl-C to detach.\n")
    _run(f"{ssh} 'nohup bash /tmp/train.sh > /home/ubuntu/train.log 2>&1 &'")
    _run(f"{ssh} 'tail -f /home/ubuntu/train.log'")


# ── SageMaker mode ─────────────────────────────────────────────────────────────

SAGEMAKER_ENTRY = textwrap.dedent("""\
    import subprocess, sys, os

    def train():
        jar = "/opt/ml/code/target/scala-2.12/mahjong-assembly-0.1.0.jar"
        out = os.environ.get("SM_OUTPUT_DATA_DIR", "/opt/ml/output/data")
        total_games = int(os.environ.get("TOTAL_GAMES", "100000"))

        # Build jar if not present
        if not os.path.exists(jar):
            subprocess.run(["sbt", "assembly"], cwd="/opt/ml/code", check=True)

        subprocess.run([
            sys.executable, "/opt/ml/code/rl/self_play.py",
            "--jar", jar,
            "--total-games", str(total_games),
            "--device", "cuda",
            "--save-dir", out,
        ], check=True)

    if __name__ == "__main__":
        train()
""")


def launch_sagemaker(args: argparse.Namespace) -> None:
    import boto3           # type: ignore
    import sagemaker       # type: ignore
    from sagemaker.pytorch import PyTorch  # type: ignore

    sess    = sagemaker.Session()
    s3_uri  = f"s3://{args.s3_bucket}/mahjong-rl"

    # Write entry point
    entry_dir = PROJECT_ROOT / "rl" / "_sm_entry"
    entry_dir.mkdir(exist_ok=True)
    (entry_dir / "train.py").write_text(SAGEMAKER_ENTRY)

    # Upload source
    print("Uploading source to S3 …")
    source_s3 = sess.upload_data(
        str(PROJECT_ROOT),
        bucket=args.s3_bucket,
        key_prefix="mahjong-rl/source",
    )
    print(f"Source uploaded → {source_s3}")

    estimator = PyTorch(
        entry_point="train.py",
        source_dir=str(entry_dir),
        role=args.role_arn,
        instance_count=1,
        instance_type=args.instance_type,
        framework_version="2.0.1",
        py_version="py310",
        environment={"TOTAL_GAMES": str(args.total_games)},
        output_path=s3_uri,
        base_job_name="mahjong-rl",
    )

    print(f"Launching SageMaker training job on {args.instance_type} …")
    estimator.fit({"code": source_s3}, wait=True, logs="All")

    model_artifacts = estimator.model_data
    print(f"\nTraining complete!  Model artifacts → {model_artifacts}")
    print(f"Download with:  aws s3 cp {model_artifacts} rl/checkpoints/sm_model.tar.gz")


# ── CLI ───────────────────────────────────────────────────────────────────────

def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(description="Launch Mahjong RL training on AWS")
    p.add_argument("--mode", choices=["ec2", "sagemaker"], required=True)
    p.add_argument("--total-games",    type=int,   default=100_000)
    p.add_argument("--instance-type",  type=str,   default="p3.2xlarge")

    # EC2 options
    ec2 = p.add_argument_group("EC2 options")
    ec2.add_argument("--key-name",        type=str,   default="my-keypair")
    ec2.add_argument("--security-group",  type=str,   default="")
    ec2.add_argument("--region",          type=str,   default="us-east-1")
    ec2.add_argument("--ami",             type=str,   default=None,
                     help="Override the AMI ID (default: Ubuntu 22.04 per region)")

    # SageMaker options
    sm = p.add_argument_group("SageMaker options")
    sm.add_argument("--role-arn",   type=str, default="")
    sm.add_argument("--s3-bucket",  type=str, default="")

    return p.parse_args()


def main() -> None:
    args = parse_args()
    if args.mode == "ec2":
        launch_ec2(args)
    elif args.mode == "sagemaker":
        launch_sagemaker(args)


if __name__ == "__main__":
    main()
