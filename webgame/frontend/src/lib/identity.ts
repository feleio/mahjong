// Client identity persisted in localStorage.

const TOKEN_KEY = "mj_token";
const NAME_KEY = "mj_name";

export function getToken(): string {
  if (typeof window === "undefined") return "";
  let token = window.localStorage.getItem(TOKEN_KEY);
  if (!token) {
    token = crypto.randomUUID();
    window.localStorage.setItem(TOKEN_KEY, token);
  }
  return token;
}

export function getName(): string {
  if (typeof window === "undefined") return "";
  return window.localStorage.getItem(NAME_KEY) ?? "";
}

export function setName(name: string): void {
  if (typeof window === "undefined") return;
  window.localStorage.setItem(NAME_KEY, name.trim());
}
