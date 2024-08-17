import { generateReports } from "../lazy-fm.js";
import * as DB from "../db.js";
import { PlayerWithRoles } from "../ps-lazy-fm.js";
import { renderScore } from "./players.js";

function renderRole([name, details]: PlayerWithRoles["roles"][0]) {
  const { txt: scoreLabel, colour: scoreColour } = renderScore(details.score);

  return `
      <div class="row">
        <div>${name}</div>
        <div class="center">${details.group}</div>
        <div></div>
        <div class="center" style="background-color: ${scoreColour}">${scoreLabel}</div>
        <div></div>
      </div>
    `;
}

function groupOrder(name: string): number {
  switch (name) {
    case "GK":
      return 0;
    case "Def":
      return 1;
    case "Mid":
      return 2;
    case "Att":
      return 3;
  }

  throw new Error(`Unknown group '${name}'`);
}

function render(playerDetails: PlayerWithRoles): string {
  const roles = playerDetails.roles
    .slice()
    .sort((a, b) => groupOrder(a[1].group) - groupOrder(b[1].group));

  return `<h4>${playerDetails.name}</h4>
    <div class="table" style="grid-template-columns: 1fr 4em 1fr 3em 1fr">
      <div class="row header">
        <div>Role</div>
        <div>Group</div>
        <div>Positions</div>
        <div>Score</div>
        <div>Attrs</div>
      </div>
      ${roles.map(renderRole).join("\n")}
    </div>
    `;
}

export class PlayerDetailsComponent extends HTMLElement {
  constructor() {
    super();
  }

  connectedCallback() {
    const reports = generateReports({
      leagueStandards: DB.readStandards(),
      players: DB.loadPlayers(),
    })();

    const uid = this.getAttribute("data-uid");
    if (uid === null) throw new Error(`Can't parse uid from element`);

    const roles = reports.playerRoles.find((p) => p.uid === uid);
    if (roles === undefined) throw new Error(`Can't find roles for '${uid}'`);

    this.innerHTML = render(roles);
  }
}

customElements.define("fm-player-details", PlayerDetailsComponent);
