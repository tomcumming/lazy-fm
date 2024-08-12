import { generateReports } from "../lazy-fm.js";

import * as DB from "../db.js";

class App extends HTMLElement {
  constructor() {
    super();

    this.innerHTML = `
      <nav>
        <a href="#input-standards">Input Division Standards</a>
        <a href="#import-players">Import Player Data</a>
        <a href="#players">Players Report</a>
      </nav>
      <div class="wrapper"></div>
    `;
  }

  connectedCallback() {
    this.refresh();

    window.addEventListener("hashchange", this.refresh.bind(this));
  }

  refresh() {
    const wrapper = this.querySelector(":scope > .wrapper");
    if (wrapper instanceof HTMLElement) {
      switch (window.location.hash) {
        case "#players":
          wrapper.innerHTML = "<fm-players />";
          break;
        case "#import-players":
          wrapper.innerHTML = "<fm-player-import />";
          break;
        case "#input-standards":
          wrapper.innerHTML = "<fm-input-standards />";
          break;
        default:
          console.error("Unknown hash route:", window.location.hash);
          break;
      }
    }
  }
}

customElements.define("fm-app", App);
