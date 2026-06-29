import re

def add_year_from_date(bib_path, output_path=None):
    """
    Adds 'year' field to each BibTeX entry if it's missing,
    extracting it from 'date' if available.
    """
    with open(bib_path, "r", encoding="utf-8") as f:
        bib_content = f.read()

    # Split entries (rough but works for most .bib structures)
    entries = re.split(r'\n@', bib_content)
    updated_entries = []
    changes = 0

    for i, entry in enumerate(entries):
        if not entry.strip():
            continue
        if i > 0 and not entry.startswith("@"):
            entry = "@" + entry

        # Check for existing year and date fields
        has_year = re.search(r'\byear\s*=\s*[{"]?\d{4}', entry)
        date_match = re.search(r'\bdate\s*=\s*[{"]?(\d{4})', entry)

        if not has_year and date_match:
            year = date_match.group(1)
            # Insert 'year' field before the last '}' or after 'date'
            entry_lines = entry.strip().splitlines()
            for j, line in enumerate(entry_lines):
                if "date" in line:
                    entry_lines.insert(j + 1, f"  year = {{{year}}},")
                    changes += 1
                    break
            entry = "\n".join(entry_lines)

        updated_entries.append(entry)

    result = "\n\n".join(updated_entries)

    output_path = output_path or bib_path
    with open(output_path, "w", encoding="utf-8") as f:
        f.write(result)

    print(f"✅ Done! Added {changes} missing 'year' fields.")
    if output_path != bib_path:
        print(f"Updated file saved to {output_path}")

# Example usage:
add_year_from_date("bibliography.bib")
