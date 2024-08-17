export function generateReports(inputArgs: {
  leagueStandards: object;
  players: object;
}): () => Reports;

export const dataTypes: {
  standardGroupAttrs: [string, string[]][];
  standardGroupNames: [string, string][];
  attrs: [string, string][];
};

export type Reports = {
  bestPositions: BestPosition[];
  playerRoles: PlayerWithRoles[];
};

export type BestPosition = {
  uid: string;
  name: string;
  positions: [
    string,
    [
      string,
      {
        role: string;
        score: number;
      },
    ][],
  ][];
};

export type PlayerWithRoles = {
  uid: string;
  name: string;
  roles: [
    string,
    {
      canPlay: [string, string][];
      group: string;
      score: number;
      attrs: [string, [number, number]][];
    },
  ][];
};
