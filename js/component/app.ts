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
      if (window.location.hash === "#players") {
        wrapper.innerHTML = "<fm-players />";
      } else if (window.location.hash === "#import-players") {
        wrapper.innerHTML = "<fm-player-import />";
      } else if (window.location.hash === "#input-standards") {
        wrapper.innerHTML = "<fm-input-standards />";
      } else if (window.location.hash.startsWith("#player-details/")) {
        const uid = window.location.hash.slice("#player-details/".length);
        wrapper.innerHTML = `<fm-player-details data-uid="${uid}" />`;
      } else {
        console.error(`Unhandled route:`, window.location.hash);
      }
    }
  }
}

customElements.define("fm-app", App);
