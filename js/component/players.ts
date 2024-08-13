import * as DB from "../db";
import { generateReports } from "../lazy-fm";
import { BestPosition } from "../ps-lazy-fm";

function renderPlayerRow(
  groups: [string, string[]][],
  bestPosition: BestPosition,
): string {
  const roleMap = new Map(
    bestPosition.positions.map(([r, cs]) => [r, new Map(cs)]),
  );

  const groupCells = groups
    .map(([r, cs]) => {
      const ces = cs
        .map((c) => {
          const best = roleMap.get(r)?.get(c);
          if (best === undefined) return `<div></div>`;
          else {
            const style = `background-color: hsl(${best.score * 100}, 100%, 75%)`;

            const scoreLabel = Math.min(0.99, best.score).toFixed(2).slice(2);

            return `<div class="center" style="${style}" title="${best.role}">
              ${scoreLabel}
            </div>`;
          }
        })
        .join("\n");

      return `
        <div class="spacer"></div>
        ${ces}
      `;
    })
    .join("\n");

  return `<div class="row">
      <div>${bestPosition.name}</div>
      ${groupCells}
    </div>`;
}

function render(bestPositions: BestPosition[]): string {
  const allCols = ["L", "C", "R"];

  const groups: [string, string[]][] = [
    ["GK", ["C"]],
    ["D", allCols],
    ["DM", allCols],
    ["M", allCols],
    ["AM", allCols],
    ["ST", ["C"]],
  ];

  const groupHeaders = groups
    .map(([r, cs]) => {
      const ces = cs
        .map(
          (c) => `
      <div>${r} ${c}</div>
      `,
        )
        .join("\n");
      return `
        <div class="spacer"></div>
        ${ces}
      `;
    })
    .join("\n");

  const templateCols = `grid-template-columns: 2fr `.concat(
    groups
      .map(([_r, cs]) => {
        return `1em `.concat(cs.map((_c) => "3em").join(" "));
      })
      .join(" "),
  );

  const playerRows = bestPositions
    .map((bp) => renderPlayerRow(groups, bp))
    .join("\n");

  return `
      <div class="table player-positions" style="${templateCols}">
        <div class="row header">
          <div>Name</div>
          ${groupHeaders}
        </div>
        ${playerRows}
      </div>
    `;
}

export class PlayersList extends HTMLElement {
  constructor() {
    super();
  }
  connectedCallback() {
    const reports = generateReports({
      leagueStandards: DB.readStandards(),
      players: DB.loadPlayers(),
    })();
    this.innerHTML = render(reports.bestPositions);
  }
}

customElements.define("fm-players", PlayersList);
