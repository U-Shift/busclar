import re

def remove_note_fields(entry):
    """
    Removes 'note' fields (including multiline and nested braces) from a BibTeX entry.
    """
    pattern = re.compile(r'^[ \t]*note\s*=\s*', re.MULTILINE)
    n = len(entry)
    i = 0
    res = []
    removed = 0

    while i < n:
        m = pattern.search(entry, i)
        if not m:
            res.append(entry[i:])
            break
        res.append(entry[i:m.start()])
        removed += 1
        pos = m.end()

        if pos < n and entry[pos] == '{':
            depth = 1
            pos += 1
            while pos < n and depth > 0:
                if entry[pos] == '{':
                    depth += 1
                elif entry[pos] == '}':
                    depth -= 1
                pos += 1
        elif pos < n and entry[pos] == '"':
            pos += 1
            while pos < n:
                if entry[pos] == '"' and entry[pos - 1] != '\\':
                    pos += 1
                    break
                pos += 1
        else:
            while pos < n and entry[pos] not in ',\n}':
                pos += 1

        while pos < n and entry[pos] in ' \t':
            pos += 1
        if pos < n and entry[pos] == ',':
            pos += 1
        while pos < n and entry[pos] in ' \t\r':
            pos += 1
        if pos < n and entry[pos] == '\n':
            pos += 1
        i = pos

    return ''.join(res), removed


def copy_journaltitle_to_journal(entry):
    """
    If 'journaltitle' is present but 'journal' is missing, adds 'journal'
    with the same value directly after 'journaltitle'.
    """
    # Check if entry already has journal
    if re.search(r'^[ \t]*journal\s*=', entry, re.MULTILINE):
        return entry, False

    # Find journaltitle field
    pattern = re.compile(r'^([ \t]*)journaltitle\s*=\s*', re.MULTILINE)
    m = pattern.search(entry)
    if not m:
        return entry, False

    indent = m.group(1)
    pos = m.end()
    val_start = pos
    n = len(entry)

    if pos < n and entry[pos] == '{':
        depth = 1
        pos += 1
        while pos < n and depth > 0:
            if entry[pos] == '{':
                depth += 1
            elif entry[pos] == '}':
                depth -= 1
            pos += 1
        value = entry[val_start:pos]
    elif pos < n and entry[pos] == '"':
        pos += 1
        while pos < n:
            if entry[pos] == '"' and entry[pos - 1] != '\\':
                pos += 1
                break
            pos += 1
        value = entry[val_start:pos]
    else:
        while pos < n and entry[pos] not in ',\n}':
            pos += 1
        value = entry[val_start:pos].strip()

    # Move to end of current line
    line_end = entry.find('\n', pos)
    if line_end == -1:
        line_end = n

    insert_pos = line_end + 1
    new_field = f"{indent}journal = {value},\n"
    updated_entry = entry[:insert_pos] + new_field + entry[insert_pos:]
    return updated_entry, True


def fix_references(bib_path, output_path=None, add_year=True, delete_notes=True, add_journal=True):
    """
    Processes BibTeX entries:
    - Adds 'year' field extracted from 'date' if 'year' is missing.
    - Deletes 'note' fields if delete_notes is True.
    - Adds 'journal' field copied from 'journaltitle' if 'journal' is missing and 'journaltitle' exists.
    """
    with open(bib_path, "r", encoding="utf-8") as f:
        bib_content = f.read()

    # Split entries (rough but works for most .bib structures)
    entries = re.split(r'\n@', bib_content)
    updated_entries = []
    years_added = 0
    notes_deleted = 0
    journals_added = 0

    for i, entry in enumerate(entries):
        if not entry.strip():
            continue
        if i > 0 and not entry.startswith("@"):
            entry = "@" + entry

        if delete_notes:
            entry, removed = remove_note_fields(entry)
            notes_deleted += removed

        if add_journal:
            entry, added = copy_journaltitle_to_journal(entry)
            if added:
                journals_added += 1

        if add_year:
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
                        years_added += 1
                        break
                entry = "\n".join(entry_lines)

        updated_entries.append(entry)

    result = "\n\n".join(updated_entries)

    output_path = output_path or bib_path
    with open(output_path, "w", encoding="utf-8") as f:
        f.write(result)

    print(
        f"✅ Done! Added {years_added} missing 'year' fields, "
        f"removed {notes_deleted} 'note' fields, "
        f"and added {journals_added} 'journal' fields from 'journaltitle'."
    )
    if output_path != bib_path:
        print(f"Updated file saved to {output_path}")


# Backward compatibility alias
add_year_from_date = fix_references

# Example usage:
fix_references("bibliography.bib")
