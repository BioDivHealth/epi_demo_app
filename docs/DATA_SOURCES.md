# Data sources and licences (place library)

The place library (`locations/library/`, built by `prepare_place_library.R`) is derived data: simplified admin boundaries, adjacency, and population totals per unit. The app code is MIT; **the data keeps its sources' licences** and must be shared with the credits below.

| Source | Used for | Licence | Credit |
|---|---|---|---|
| [geoBoundaries](https://www.geoboundaries.org) CGAZ ADM1/ADM2 and gbOpen ADM3 | Unit boundaries | **Varies by country**: mostly ODbL 1.0, CC BY 3.0 IGO / 4.0, public domain; some CC BY-SA | Runfola, D. et al. (2020) geoBoundaries: A global database of political administrative boundaries. *PLoS ONE* 15(4): e0231866 |
| [GHS-POP R2023A](https://ghsl.jrc.ec.europa.eu) 2020, 30 arcsec | Population per unit | CC BY 4.0 | Schiavina, M. et al. (2023) GHS-POP R2023A. European Commission, Joint Research Centre |
| [WorldPop](https://www.worldpop.org) 2020 1 km UN-adjusted | Portugal example only | CC BY 4.0 | WorldPop (www.worldpop.org), University of Southampton |
| [Natural Earth](https://www.naturalearthdata.com) | Country names/outlines, city points, urban area size | Public domain | Made with Natural Earth |

## What this means in practice

- **Per-place licence:** `locations/library/LICENSES.csv` lists the boundary licence for every place. Places whose boundaries need direct permission from the provider are left out of the library.
- **Share-alike:** places built from ODbL or CC BY-SA boundaries are themselves under those terms. If you redistribute the library, keep it under the same licence as the source data, keep this credit file with it, and keep it separate from the MIT-licensed code.
- **Attribution in the app:** show a short credit line (e.g. "Boundaries: geoBoundaries; population: GHS-POP (JRC)") wherever the maps appear.

This is a summary to help reuse; it is not legal advice. Check `LICENSES.csv` and the geoBoundaries metadata before publishing the data.
