# Project City

## Setup
1. Put this whole folder somewhere on your machine, open it as your working
   directory in RStudio (or open a new RStudio Project in this folder).
2. Open `app.R` and click **Run App**.
3. Required packages (shiny, shinyjs, DBI, RSQLite, dplyr, openssl, qrcode,
   jsonlite) install automatically on first run if missing.
4. Optional packages - only needed for certain export formats, app runs fine
   without them:
   - `openxlsx` -> enables Excel export
   - `officer` -> enables Word export
   - `pagedown` (+ a local Chrome/Chromium install) -> enables PDF export
   Install any of these yourself if you want those buttons active:
   `install.packages(c("openxlsx","officer","pagedown"))`

## What's fully built
- Register/login, multi-page nav (Dashboard, Map, District Detail, Activity,
  Export, Settings) with a persistent top bar
- Multiple projects per user, City Map with empty state + "+ Add District"
- District Detail: checklist, tagged notes (earthy palette, auto icon badges
  for dates/code), status + due date (auto flips to "Overdue" badge), linked
  resources (URLs + file upload), per-district activity log
- Dashboard: overall progress %, overdue callout, recently updated list
- Search box on the City Map (filters by district name and note content)
- Settings: profile fields, dark mode toggle, project rename/delete,
  2FA setup/disable
- Exports: HTML and Markdown always on; CSV and JSON always on; Excel/Word/PDF
  guarded behind the optional packages above
- Caramel theme throughout; earthy "Vintage Matcha" palette scoped only to
  the notes panel, as agreed

## Please test carefully before relying on it
- **2FA (TOTP)**: this is a from-scratch RFC 6238 implementation (base32 +
  HMAC-SHA1 + dynamic truncation) - there's no mature R package for this, so
  it's the one piece of the app I'd genuinely stress-test. Enable it on a
  throwaway test account first, confirm the code from Google Authenticator/
  Authy actually matches and logs you in, before trusting it on your real
  account.
- **PDF export** depends on a local Chrome/Chromium install being found by
  `pagedown::chrome_print()` - if it's missing, the button will show a
  friendly error notification rather than crashing the app.
- I wrote and reviewed this code carefully but could not execute R in the
  environment I built it in, so treat this as a strong first build to debug
  in RStudio rather than a guaranteed zero-error result.

## Not included (by earlier decisions)
- Audio-to-note transcription (deferred - conflicts with offline-first design)
- Embedded website browsing / iframes
- Full undo system, multi-user collaboration
