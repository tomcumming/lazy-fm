import * as DB from "../db";
import { generateReports } from "../lazy-fm";
import { PlayerWithRoles } from "../ps-lazy-fm";
import { renderScore } from "./players";

function renderTable(roleId: string, players: PlayerWithRoles[]): string {
  const playerRows = players.map((player) => {
    const role = player.roles.find(([name, _]) => name === roleId);
    if (role === undefined) throw new Error(`Can't find role for player`);

    const [_, { canPlay, score }] = role;
    const poss = canPlay.map(([r, c]) => `${r}${c}`).join(", ");
    const rs = renderScore(score);

    return `<div class="row">
      <div data-uid="${player.uid}">${player.name}</div>
      <div>${poss}</div>
      <div style="background-color: ${rs.colour}">${rs.txt}</div>
      </div>`;
  });

  return `
      <div class="table player-positions" style="grid-template-columns: 1fr 1fr 3em">
        <div class="row header">
          <div>Name</div>
          <div>Able</div>
          <div>Score</div>
        </div>
        ${playerRows.join("\n")}
      </div>
    `;
}

function render(roleId: null | string, players: PlayerWithRoles[]) {
  const table =
    roleId === null ? `<div>Select a role</div>` : renderTable(roleId, players);

  const roles = players[0].roles.map(
    ([name, _]) => `<option
        ${name === roleId ? "selected" : ""}
      >
        ${name}
      </option>`,
  );

  return `<select>
      ${roles.join("\n")}
    </select>
    ${table}
    `;
}

export class RolesList extends HTMLElement {
  constructor() {
    super();

    this.addEventListener("change", () => this.onChange());
  }
  connectedCallback() {
    const reports = generateReports({
      leagueStandards: DB.readStandards(),
      players: DB.loadPlayers(),
    })();

    const roleId = this.getAttribute("data-role");

    this.innerHTML = render(roleId, reports.playerRoles);
  }

  onChange() {
    const selectElem = this.querySelector("select");
    if (!selectElem) throw new Error(`Can't find select elem`);

    const selected = selectElem.value;
    window.location.hash = `#roles/${selected}`;
  }
}

customElements.define("fm-roles", RolesList);
