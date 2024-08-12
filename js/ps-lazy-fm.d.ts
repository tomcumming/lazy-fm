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
};

export type BestPosition = {
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
