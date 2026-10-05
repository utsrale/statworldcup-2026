# Project Brief: FIFA World Cup Hub (R-Shiny Implementation)

## 1. Project Overview
The **FIFA World Cup Hub** is a premium, interactive dashboard designed to provide fans and analysts with a high-fidelity tournament experience. Built as an R-Shiny application, it combines real-time data, predictive modeling (Bracket Simulator), and advanced performance analytics (H2H Radar Charts).

## 2. Visual Identity & Design System
The project follows the **Elite Tournament Core** design system, prioritizing a professional, sporty, and modern aesthetic.

*   **Theme:** Premium Dark Mode with Glassmorphism.
*   **Primary Palette:** 
    *   **Qatar Maroon (#8a1538):** Primary brand color for highlights and CTAs.
    *   **Trophy Gold:** Accents for winners, ranks, and premium elements.
    *   **Surface Dark (#121318):** Base background for high contrast.
*   **Typography:** Montserrat (San-serif) for all headings and body text to ensure readability and a modern feel.
*   **Effects:** Backdrop blurs (12px-20px), subtle borders (border-glass), and high-elevation shadows for interactive cards.

## 3. Core Features & Screen Modules

### A. Dashboard Utama (Main Hub)
*   **Hero Match:** Real-time score tracking with team crests and match status.
*   **Group Standings:** Compact, scrollable grid of Group A-H standings including points (PTS).
*   **Match Schedule:** Chronological list of upcoming and past matches with venue and status details.

### B. Bracket Predictor (Simulator)
*   **Interactive Flow:** Visual tournament tree from Round of 16 to the Final.
*   **Live Probabilities:** AI-driven sidebar showing "Winning Confidence" and "Prediction Accuracy" based on user selections.
*   **Hot Take AI:** Dynamic commentary box that analyzes the user's predicted bracket path.

### C. H2H Comparison (Analytics)
*   **Performance Matrix:** Interactive Radar Chart (Plotly-based) comparing Attack, Defense, Speed, Physical, and Experience.
*   **Key Player Matchups:** Side-by-side player cards with individual stats.
*   **Historical Data:** Table of past tournament meetings between the selected nations.

### D. Stadium Map (Geospatial)
*   **Leaflet Integration:** Interactive map with custom markers for all tournament venues.
*   **Venue Details:** Side panel showing stadium capacity, city, and real-time weather data (Temperature/Humidity).
*   **Logistics:** "View Route" functionality for travel planning.

## 4. Technical Architecture (R-Shiny)
*   **Frontend:** `fluidPage` with custom CSS (`www/custom.css`) for Glassmorphism. Navigation handled via a custom `SideNavBar` module.
*   **Backend:** 
    *   `reactiveValues` for managing bracket state and user selections.
    *   `plotly` for the Radar Charts.
    *   `leaflet` for the geographic visualizations.
*   **Data Strategy:** Hybrid approach using local JSON/CSV for high-speed fallback and `httr`/`jsonlite` for live API updates (matches/weather).

## 5. Success Metrics
*   **Deployability:** 100% compatibility with shinyapps.io.
*   **Interactivity:** <200ms latency for reactive UI updates (Bracket clicks/H2H selection).
*   **Visual Fidelity:** Precise replication of the Glassmorphism effects across all browser resolutions.