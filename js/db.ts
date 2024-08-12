const STANDARDS_KEY = "standards";
const PLAYERS_KEY = "players";

export function saveStandards(standards: object) {
  localStorage.setItem(STANDARDS_KEY, JSON.stringify(standards));
}

export function readStandards(): object {
  const json = localStorage.getItem(STANDARDS_KEY);
  return json === null ? {} : JSON.parse(json);
}

export function savePlayers(players: object[]) {
  localStorage.setItem(PLAYERS_KEY, JSON.stringify(players));
}

export function loadPlayers(): object[] {
  const json = localStorage.getItem(PLAYERS_KEY);
  return json === null ? [] : JSON.parse(json);
}
