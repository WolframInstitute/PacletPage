(* ::Package:: *)

(* ==================== the feedback system ====================

   The feedback of the site name lives in the directory <name>Feedback of the publishing account:

     Form          a FormFunction, public. What is wrong, unclear or missing on the page; where
                   on the page, what the reader expected and where to reply, optionally; up to
                   three files; and five fields with the page's identity (its id, title, link in
                   the site, the view and the notebook's URL), which the host page's panel fills
                   in. Each submission becomes an entry of a Databin, and the form answers with a
                   page the panel recognizes by its #pp-feedback-thanks element.
     attachments/  the uploaded files, private, named by the entry they came with.
     All           the team's page listing the entries, newest first, read each time it opens.
     File          the attachments, one at a time (File?f=<name>), to whoever may open All.
     status        private: where each entry stands, by its ID; an entry it does not name is open.
     SetStatus     changes an entry's status, from a Wolfram kernel signed in as a team member.
     Me            the signed-in reader's Wolfram ID, to prefill the panel's email field: only for
                   requests from the site's own pages, never cached.
     config        private: the Databin's id, so a redeploy keeps writing to the same bin.

   All, File and SetStatus let only signed-in Wolfram IDs reach them ("Authenticated"), and their
   code then admits the publishing account, the IDs listed as readers and every ID of the listed
   email domains; cloud permissions cannot name a domain, so that check is in the code. The cloud
   form's security token is off, since the panel posts to the form directly; a honeypot field
   ("Website", which people leave empty) makes a robot's submission a thank-you that stores
   nothing, every field is clipped to a fixed length, and a file over 20 MB is dropped. *)

feedbackDirectory[name_String] := siteURL[name] <> "Feedback";

$contextFields = {"Page", "Title", "Link", "View", "Notebook"};

$attachmentFields = {"Attachment1", "Attachment2", "Attachment3"};

$statusValues = {"open", "decided", "fixed", "follow-up", "not a defect"};

$maxAttachmentBytes = 20 * 10^6;

(* a text answer, trimmed and clipped to n characters; no answer is the empty string *)
clip[text_String, n_Integer] := StringTake[StringTrim[text], UpTo[n]];
clip[_Missing | Null, _Integer] := "";
clip[other_, n_Integer] := clip[TextString[other], n];

(* an answer's text: a browser posts the form as multipart data, whose text the cloud hands over
   with each byte of its UTF-8 encoding as a character of its own; repairText reads them back *)
answerText[value_, n_Integer] := clip[repairText[value], n];

(* a string as Wolfram Language code *)
wlString[s_String] := "\"" <> StringReplace[s, {"\\" -> "\\\\", "\"" -> "\\\""}] <> "\"";

(* ==================== a submission ==================== *)

(* the extension a file's format gives it, for an upload that arrived without its name *)
formatExtension[format_] := Replace[format, {
    "PNG" -> ".png", "JPEG" -> ".jpg", "GIF" -> ".gif", "WebP" -> ".webp", "PDF" -> ".pdf",
    "Text" -> ".txt", "CSV" -> ".csv", "NB" -> ".nb", "Package" -> ".wl", "ZIP" -> ".zip",
    _ -> ""
}];

localFileQ[value_] := MatchQ[value, File[_String]] && TrueQ[FileExistsQ[value]];

(* the name an attachment is stored under: the name the panel sent for the k-th file, else the one
   the request carries, else "attachment-k" with the extension of its format; as a safe file name *)
attachmentName[k_Integer, sentNames_List, meta_Association, value_] := Module[{sent, original},
    sent = If[Length[sentNames] >= k, sentNames[[k]], ""];
    original = Which[
        sent =!= "", sent,
        MatchQ[Lookup[meta, "OriginalFileName"], _String?(StringLength[#] > 0 && ! StringStartsQ[#, "/"] &)], meta["OriginalFileName"],
        True, "attachment-" <> IntegerString[k] <> formatExtension[If[localFileQ[value], FileFormat[value], None]]
    ];
    StringTake[StringReplace[FileNameTake[original], Except[WordCharacter | "." | "-"] -> "_"], UpTo[80]]
];

(* the uploaded file, copied into the attachments directory for the account alone; its URL, or
   Nothing for no upload, one over the size limit, or a failed copy *)
storeAttachment[value_, name_String, key_String, directory_String] := Module[{object},
    object = If[
        MatchQ[value, _CloudObject] || (localFileQ[value] && FileByteCount[value] <= $maxAttachmentBytes),
        CopyFile[value, CloudObject[directory <> "/" <> key <> "-" <> name, Permissions -> "Private"]],
        None
    ];
    If[MatchQ[object, _CloudObject], First[object], Nothing]
];

requestData[] := Replace[HTTPRequestData[], Except[_Association] -> <||>];

toAssociation[a_Association] := a;
toAssociation[rules : {___Rule}] := Association[rules];
toAssociation[_] := <||>;

(* the request's parts, and the metadata of one *)
requestParts[] := toAssociation[Lookup[requestData[], "MultipartElements", <||>]];
partMeta[parts_Association, field_String] := toAssociation[Lookup[parts, field, <||>]];

(* the request's headers, names in lower case *)
requestHeaders[] := Association @ KeyValueMap[ToLowerCase[TextString[#1]] -> TextString[#2] &,
    toAssociation[Lookup[requestData[], "Headers", <||>]]];

(* styled by class: the panel recognizes a stored submission by the id of its element alone *)
feedbackStyle = "body{margin:0;font:15px/1.5 'Source Sans Pro',-apple-system,sans-serif;color:#333}\
.pp-box{max-width:460px;margin:56px auto;padding:0 20px;text-align:center}\
h1{font-size:20px;font-weight:600;margin:0 0 8px}";

feedbackHTML[title_String, id_String, heading_String, text_String] := StringJoin[
    "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    "<title>", esc[title], "</title><style>", feedbackStyle, "</style></head><body>",
    "<div class=\"pp-box\" id=\"", id, "\"><h1>", esc[heading], "</h1><p>", esc[text], "</p></div></body></html>"
];

(* The Databin keeps text as bytes, and when the text is not ASCII it answers DatabinAdd with JSON
   that is not UTF-8, so DatabinAdd fails although the entry is stored. Text is stored as ASCII:
   each character past ASCII as \uXXXX and each backslash doubled, in an entry whose "Encoding"
   says so. *)
$storedEncoding = "escaped";

toStoredText[s_String] := StringReplace[s, {"\\" -> "\\\\", c : RegularExpression["[^\\x00-\\x7F]"] :> jsonCharacter[c]}];
toStoredText[other_] := other;

hex4 = Repeated[HexadecimalCharacter, {4}];

fromStoredText[s_String] := StringReplace[s, {
    "\\\\" -> "\\",
    "\\u" ~~ hi : hex4 ~~ "\\u" ~~ lo : hex4 /; 55296 <= FromDigits[hi, 16] <= 56319 && 56320 <= FromDigits[lo, 16] <= 57343 :>
        FromCharacterCode[65536 + 1024 (FromDigits[hi, 16] - 55296) + FromDigits[lo, 16] - 56320],
    "\\u" ~~ h : hex4 :> FromCharacterCode[FromDigits[h, 16]]
}];
fromStoredText[other_] := other;

(* an entry as it was submitted: decoded when it was stored as ASCII; an entry stored as raw
   text, or received as UTF-8 bytes and stored before they were read back, comes back with those
   bytes as characters, which repairText reads *)
storedEntry[entry_Association] := Map[toStoredText, Append[entry, "Encoding" -> $storedEncoding]];

readEntry[entry_Association] := If[ Lookup[entry, "Encoding"] === $storedEncoding,
    Map[repairText[fromStoredText[#]] &, KeyDrop[entry, "Encoding"]],
    Map[repairText, entry]
];

htmlResponse[html_String, code_Integer] := HTTPResponse[html,
    <|"StatusCode" -> code, "ContentType" -> "text/html; charset=utf-8", "CharacterEncoding" -> "UTF-8"|>];

plainResponse[code_Integer, text_String] := HTTPResponse[text, <|"StatusCode" -> code, "ContentType" -> "text/plain; charset=utf-8"|>];

(* one submission: its entry, with the ID the team's page locates it by, stored unless the
   honeypot was filled; a robot is thanked like anyone else and nothing is stored *)
receiveFeedback[answers_Association, bin_String, attachments_String, title_String, email_String] := Module[
    {key, parts, sentNames, stored, entry},
    key = DateString[TimeZoneConvert[Now, 0], {"Year", "Month", "Day", "-", "Hour", "Minute", "Second"}] <> "-" <> StringTake[CreateUUID[], 8];
    parts = requestParts[];
    sentNames = StringTrim @ StringSplit[answerText[answers["AttachmentNames"], 2000], "\n"];
    entry = Join[
        <|
            "ID" -> key,
            "Source" -> "Feedback panel",
            "Submitted" -> DateString[TimeZoneConvert[Now, 0], {"ISODate", "T", "Time"}] <> "Z",
            "Feedback" -> answerText[answers["Feedback"], 5000],
            "Where" -> answerText[answers["Where"], 500],
            "Expected" -> answerText[answers["Expected"], 2000],
            "Email" -> answerText[answers["Email"], 200]
        |>,
        AssociationMap[answerText[answers[#], 500] &, $contextFields]
    ];
    stored = clip[answers["Website"], 10] =!= "" || MatchQ[
        DatabinAdd[Databin[bin], storedEntry @ Append[entry, "Attachments" -> StringRiffle[
            MapIndexed[
                storeAttachment[answers[#1], attachmentName[First[#2], sentNames, partMeta[parts, #1], answers[#1]], key, attachments] &,
                $attachmentFields
            ],
            " "
        ]]],
        _Databin
    ];
    If[ stored,
        htmlResponse[feedbackHTML["Feedback sent", "pp-feedback-thanks", "Thank you", "Your feedback on the " <> title <> " documentation was sent."], 200],
        htmlResponse[feedbackHTML["Feedback not sent", "pp-feedback-failed", "Sorry, that did not go through",
            If[email === "", "Please try again later.", "Please email " <> email <> " instead."]], 500]
    ]
];

feedbackForm[bin_String, attachments_String, title_String, email_String] := FormFunction[
    Join[
        {
            "Feedback" -> <|
                "Interpreter" -> "String",
                "Label" -> "What is wrong, unclear or missing?",
                "Control" -> Function[InputField[##, FieldSize -> {48, 6}]]
            |>,
            "Where" -> <|"Interpreter" -> "String", "Required" -> False, "Label" -> "Where on the page?", "Hint" -> "a section, an example or an output"|>,
            "Expected" -> <|
                "Interpreter" -> "String",
                "Required" -> False,
                "Label" -> "What did you expect instead?",
                "Control" -> Function[InputField[##, FieldSize -> {48, 3}]]
            |>,
            "Email" -> <|"Interpreter" -> "EmailAddress", "Required" -> False, "Label" -> "Your email", "Hint" -> "only if you would like a reply"|>,
            "Attachment1" -> <|"Interpreter" -> "CachedFile", "Required" -> False, "Label" -> "Screenshot or file", "Hint" -> "up to three files; the site's Feedback panel also takes a pasted screenshot"|>,
            "Attachment2" -> <|"Interpreter" -> "CachedFile", "Required" -> False, "Label" -> "Another file"|>,
            "Attachment3" -> <|"Interpreter" -> "CachedFile", "Required" -> False, "Label" -> "Another file"|>
        },
        Map[# -> <|"Interpreter" -> "String", "Required" -> False, "Label" -> #|> &, $contextFields],
        {
            (* the cloud hands the form a copy of each upload without its name, so the panel
               sends the names alongside, one per line *)
            "AttachmentNames" -> <|"Interpreter" -> "String", "Required" -> False, "Label" -> "Attachment names"|>,
            (* the honeypot: people leave it empty, form-filling robots do not *)
            "Website" -> <|"Interpreter" -> "String", "Required" -> False, "Label" -> "Website (leave this empty)"|>
        }
    ],
    Function[answers, receiveFeedback[answers, bin, attachments, title, email]],
    FormProtectionMethod -> None,
    AppearanceRules -> <|
        "Title" -> "Feedback on this page",
        "Description" -> "What should the " <> title <> " documentation fix or improve? Everything but the first field is optional.",
        "SubmitLabel" -> "Send feedback"
    |>
];

(* ==================== the team ==================== *)

(* whether the signed-in requester is on the team: the account itself, a listed reader, or an ID
   in a listed domain; a requester's Wolfram ID is an email address *)
admittedQ[who_String, owner_String, readers_List, domains_List] := With[{id = ToLowerCase[who]},
    id === ToLowerCase[owner] || MemberQ[ToLowerCase[readers], id] ||
        AnyTrue[ToLowerCase[domains], StringEndsQ[id, "@" <> StringTrim[#, "@"]] &]
];
admittedQ[_, _, _, _] := False;

domainsText[domains_List] := StringRiffle["@" <> StringTrim[#, "@"] & /@ domains, ", ", " and "];

deniedPage[who_, title_String, domains_List] := htmlResponse[
    feedbackHTML[title <> " documentation feedback", "pp-denied", "Not for this account",
        StringJoin[
            "The feedback on the ", title, " documentation is for its team",
            If[domains === {}, "", ", including signed-in " <> domainsText[domains] <> " accounts"],
            ". You are signed in as ", TextString[who], "."
        ]],
    403
];

(* The Databin hands text back with each byte of its UTF-8 encoding as a character of its own;
   reading those codes as UTF-8 restores it. Text that is not such a byte string, or whose bytes
   are not UTF-8, is left as it is. *)
$utf8Bytes = RegularExpression["(?:[\\x00-\\x7F]|[\\xC2-\\xDF][\\x80-\\xBF]|\\xE0[\\xA0-\\xBF][\\x80-\\xBF]|[\\xE1-\\xEC\\xEE\\xEF][\\x80-\\xBF]{2}|\\xED[\\x80-\\x9F][\\x80-\\xBF]|\\xF0[\\x90-\\xBF][\\x80-\\xBF]{2}|[\\xF1-\\xF3][\\x80-\\xBF]{3}|\\xF4[\\x80-\\x8F][\\x80-\\xBF]{2})*"];

repairText[text_String] := If[
    StringContainsQ[text, RegularExpression["[\\x80-\\xFF]"]] && StringMatchQ[text, $utf8Bytes],
    FromCharacterCode[ToCharacterCode[text], "UTF-8"],
    text
];
repairText[other_] := other;

(* the cell an entry names ("cell 19"), for ordering; 0 for none *)
entryCellNumber[entry_Association] := Replace[
    StringCases[TextString[Lookup[entry, "Where", ""]], "cell " ~~ n : DigitCharacter .. :> FromDigits[n]],
    {{n_, ___} :> n, _ -> 0}
];

(* the entries, newest first; entries of one moment (an imported batch) by page, then by cell *)
feedbackEntries[bin_String] := With[{data = Dataset[Databin[bin]]},
    Catenate @ Map[
        SortBy[#, {TextString[Lookup[#, "Page", ""]], entryCellNumber[#]} &] &,
        ReverseSortBy[
            GatherBy[
                Map[readEntry, Select[If[MatchQ[data, _Dataset], Normal[data], {}], AssociationQ]],
                TextString[Lookup[#, "Submitted", ""]] &
            ],
            TextString[Lookup[First[#], "Submitted", ""]] &
        ]
    ]
];

(* the statuses, by entry ID *)
feedbackStatuses[status_String] := If[
    TrueQ[FileExistsQ[CloudObject[status]]],
    Replace[CloudGet[CloudObject[status]], Except[_Association] -> <||>],
    <||>
];

(* statuses stored in one write, each with the time it was set *)
storeStatuses[status_String, updates_List] := With[{now = DateString[TimeZoneConvert[Now, 0], "ISODateTime"]},
    CloudPut[
        Join[feedbackStatuses[status], Association @ Map[#["id"] -> <|"Status" -> #["status"], "Note" -> #["note"], "Updated" -> now|> &, updates]],
        CloudObject[status],
        Permissions -> "Private"
    ]
];

(* the statuses a request sets: one, as id, status and note, or several, as "updates", a JSON list
   of objects with id, status and note *)
statusUpdates[arguments_Association] := Which[
    StringQ[Lookup[arguments, "updates", None]],
        Replace[ImportString[arguments["updates"], "RawJSON"], {
            list : {__Association} /; AllTrue[list, StringQ[Lookup[#, "id", None]] && StringQ[Lookup[#, "status", None]] &] :>
                Map[<|"id" -> #["id"], "status" -> #["status"], "note" -> clip[Replace[Lookup[#, "note", ""], Except[_String] -> ""], 500]|> &, list],
            _ -> $Failed
        }],
    StringQ[Lookup[arguments, "id", None]] && StringQ[Lookup[arguments, "status", None]],
        {<|"id" -> arguments["id"], "status" -> arguments["status"], "note" -> clip[Replace[Lookup[arguments, "note", ""], Except[_String] -> ""], 500]|>},
    True, $Failed
];

(* ==================== the team's page ==================== *)

entryAnchor[entry_Association] := StringReplace[TextString[Lookup[entry, "ID", Lookup[entry, "Submitted", ""]]], Except[WordCharacter | "-"] -> "-"];

(* the code that finds the entry in the Databin, for the account that owns it *)
entryLocator[entry_Association, bin_String] := StringJoin[
    "SelectFirst[Dataset[Databin[", wlString[bin], "]], ",
    If[ StringQ[Lookup[entry, "ID"]],
        "#ID === " <> wlString[entry["ID"]] <> " &]",
        "#Submitted === " <> wlString[TextString[Lookup[entry, "Submitted", ""]]] <> " &]"
    ]
];

(* the code that changes an entry's status, with its current status and note to edit *)
statusSetter[setStatus_String, entryID_String, value_String, note_String] := StringJoin[
    "(* feedback entry ", entryID, ": its status, one of ", StringRiffle[wlString /@ $statusValues, ", "], ", and a note *)\n",
    "URLExecute[CloudObject[", wlString[setStatus], "], {\"id\" -> ", wlString[entryID], ", \"status\" -> ", wlString[value],
    ", \"note\" -> ", wlString[note], "}]"
];

$listColumns = {"Submitted", "Status", "Page", "Feedback", "Where", "Expected", "Attachments", "Email", "View", "Locate"};

entryCell[entry_, "Status", context_] := With[{
    entryID = TextString[Lookup[entry, "ID", ""]],
    status = Lookup[context["Statuses"], TextString[Lookup[entry, "ID", ""]], <||>]
},
    StringJoin[
        "<button type=\"button\" class=\"copy badge s-", StringReplace[ToLowerCase[Lookup[status, "Status", "open"]], Except[LetterCharacter] -> "-"],
        "\" title=\"Copy the code that changes this status\" data-copy=\"",
        esc[statusSetter[context["SetStatus"], entryID, Lookup[status, "Status", "open"], Lookup[status, "Note", ""]]], "\">",
        esc[Lookup[status, "Status", "open"]], "</button>",
        If[MatchQ[Lookup[status, "Note", ""], "" | _Missing], "", "<div class=\"note\">" <> esc[status["Note"]] <> "</div>"]
    ]
];

entryCell[entry_, "Submitted", _] := StringJoin[
    esc[StringReplace[TextString[Lookup[entry, "Submitted", ""]], {"T" -> " ", "Z" -> " UTC"}]],
    "<div class=\"id\"><a href=\"#", entryAnchor[entry], "\">#", esc[Lookup[entry, "ID", "link"]], "</a></div>",
    "<div class=\"src\">", esc[Lookup[entry, "Source", ""]], "</div>"
];

(* the feedback, with the evidence behind it when the entry carries some (an imported finding) *)
entryCell[entry_, "Feedback", _] := StringJoin[
    esc[Lookup[entry, "Feedback", ""]],
    If[ MatchQ[Lookup[entry, "Evidence", ""], "" | _Missing],
        "",
        "<details><summary>Evidence</summary><div class=\"evidence\">" <> esc[entry["Evidence"]] <> "</div></details>"
    ]
];

entryCell[entry_, "Page", context_] := With[{page = TextString[Lookup[entry, "Page", ""]], title = TextString[Lookup[entry, "Title", ""]]},
    If[ page === "",
        esc[title],
        StringJoin[
            "<a href=\"", esc[context["Site"] <> "#" <> page], "\" target=\"_blank\" rel=\"noopener\">", esc[If[title === "", page, title]], "</a>",
            "<div class=\"id\">", esc[page], "</div>"
        ]
    ]
];

(* attachments through File, beside this page; a stored name begins with the entry's
   25-character key, which the link text leaves out *)
entryCell[entry_, "Attachments", _] := StringJoin @ Map[
    Function[url,
        With[{name = Last[StringSplit[url, "/"]]},
            StringJoin[
                "<a href=\"File?f=", URLEncode[name], "\" target=\"_blank\" rel=\"noopener\">",
                If[ StringMatchQ[name, ___ ~~ "." ~~ ("png" | "jpg" | "jpeg" | "gif" | "webp"), IgnoreCase -> True],
                    "<img src=\"File?f=" <> URLEncode[name] <> "\" alt=\"" <> esc[name] <> "\">",
                    esc[StringDrop[name, Min[25, StringLength[name]]]]
                ],
                "</a>"
            ]
        ]
    ],
    StringSplit[TextString[Lookup[entry, "Attachments", ""]]]
];

entryCell[entry_, "Email", _] := With[{email = TextString[Lookup[entry, "Email", ""]]},
    If[email === "", "", "<a href=\"mailto:" <> esc[email] <> "\">" <> esc[email] <> "</a>"]
];

entryCell[entry_, "Locate", context_] := With[{locator = esc[entryLocator[entry, context["Databin"]]]},
    "<button type=\"button\" class=\"copy\" data-copy=\"" <> locator <> "\" title=\"" <> locator <> "\">Copy WL</button>"
];

entryCell[entry_, column_String, _] := esc[Lookup[entry, column, ""]];

listStyle = StringJoin[
    "body{margin:0;padding:24px;font:14px/1.45 'Source Sans Pro',-apple-system,sans-serif;color:#222;background:#fafafa}",
    "h1{font-size:22px;font-weight:600;margin:0 0 4px}.meta{color:#666;margin:0 0 6px}",
    ".bin{color:#666;margin:0 0 16px}.bin code{background:#f0f0f0;padding:1px 5px;border-radius:3px}",
    "table{border-collapse:collapse;width:100%;background:#fff;box-shadow:0 1px 3px rgba(0,0,0,.08)}",
    "th,td{text-align:left;vertical-align:top;padding:8px 10px;border-bottom:1px solid #eee}",
    "th{background:#f3f3f3;font-weight:600;position:sticky;top:0}tr:target td{background:#fff8dc}",
    "td.feedback,td.expected{white-space:pre-wrap;max-width:440px}td.submitted{white-space:nowrap;color:#555}",
    "td.attachments img{max-width:160px;max-height:110px;border:1px solid #ddd;border-radius:3px;display:block;margin:0 0 6px}",
    ".id{color:#888;font-size:12px}.id a{color:#888}a{color:#1a5fb4}",
    "button.copy{font:inherit;font-size:12px;padding:3px 8px;border:1px solid #ccc;border-radius:4px;background:#fff;cursor:pointer;white-space:nowrap}",
    "button.copy:hover{border-color:#999}",
    ".src{color:#999;font-size:11px;white-space:normal;max-width:150px}details{margin-top:6px}summary{color:#1a5fb4;cursor:pointer;font-size:12px}",
    ".evidence{white-space:pre-wrap;color:#444;font-size:13px;margin-top:4px;padding-left:8px;border-left:2px solid #e3e3e3}",
    "#filter{font:inherit;padding:6px 9px;width:min(420px,100%);border:1px solid #ccc;border-radius:4px;margin:0 8px 12px 0}#shown{color:#666}",
    "button.copy.badge{display:inline-block;margin:0;border:0;font-family:inherit;font-size:12px;line-height:20px;padding:0 8px;",
    "border-radius:10px;background:#eee;color:#555;white-space:nowrap;cursor:pointer;vertical-align:top}button.copy.badge:hover{filter:brightness(.95)}",
    "th button.sort{all:unset;cursor:pointer;font-weight:600}th button.sort:hover{color:#1a5fb4}th .arrow{color:#999;font-size:11px;margin-left:4px}",
    "button.badge.s-decided{background:#e3eefb;color:#1a5fb4}button.badge.s-fixed{background:#e3f4e6;color:#1e7b34}",
    "button.badge.s-follow-up{background:#fbf1dc;color:#8a5a00}button.badge.s-not-a-defect{background:#f2f2f2;color:#888}",
    ".note{color:#666;font-size:12px;margin-top:4px;max-width:200px}td.status{white-space:normal}"
];

(* the copy buttons, the filter and the column sort *)
listScript = StringJoin[
    "<script>document.addEventListener('click', function (e) {",
    "var b = e.target.closest && e.target.closest('button.copy'); if (!b) return;",
    "var text = b.getAttribute('data-copy'), label = b.textContent;",
    "function done(ok) { b.textContent = ok ? 'Copied' : 'Select and copy'; setTimeout(function () { b.textContent = label; }, 1500); }",
    "function legacy() { var t = document.createElement('textarea'); t.value = text; document.body.appendChild(t); t.select();",
    "var ok = false; try { ok = document.execCommand('copy'); } catch (x) {} document.body.removeChild(t); return ok; }",
    "if (navigator.clipboard) navigator.clipboard.writeText(text).then(function () { done(true); }, function () { done(legacy()); });",
    "else done(legacy()); });",
    "var f = document.getElementById('filter'), shown = document.getElementById('shown'), rows = document.querySelectorAll('tbody tr');",
    "function filter() { var q = f.value.trim().toLowerCase(), n = 0; rows.forEach(function (r) {",
    "var hit = !q || r.textContent.toLowerCase().indexOf(q) >= 0; r.style.display = hit ? '' : 'none'; if (hit) n++; });",
    "shown.textContent = q ? n + ' shown' : ''; }",
    "f.addEventListener('input', filter);",
    "var heads = document.querySelectorAll('th button.sort'), tbody = document.querySelector('tbody'), sorted = -1, dir = 1;",
    "heads.forEach(function (h, i) { h.addEventListener('click', function () {",
    "dir = sorted === i ? -dir : 1; sorted = i;",
    "var list = Array.prototype.slice.call(tbody.rows);",
    "list.sort(function (a, b) { return dir * a.cells[i].textContent.trim().localeCompare(b.cells[i].textContent.trim(), undefined, { numeric: true, sensitivity: 'base' }); });",
    "list.forEach(function (r) { tbody.appendChild(r); });",
    "heads.forEach(function (g, j) { g.querySelector('.arrow').innerHTML = j === i ? (dir > 0 ? '&#9650;' : '&#9660;') : '&#8597;'; }); }); });</script>"
];

(* context: Title, Site, Databin, SetStatus, Domains, Statuses *)
listHTML[entries_List, context_Association] := StringJoin[
    "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    "<title>", esc[context["Title"]], " documentation feedback</title><style>", listStyle, "</style></head><body>",
    "<h1>", esc[context["Title"]], " documentation feedback</h1>",
    "<p class=\"meta\">", IntegerString[Length[entries]], " entries, newest first. For the team",
    If[context["Domains"] === {}, "", ", including signed-in " <> esc[domainsText[context["Domains"]]] <> " accounts"], ".</p>",
    "<p class=\"bin\">The entries live in <code>Databin[", esc[wlString[context["Databin"]]], "]</code> ",
    "<button type=\"button\" class=\"copy\" data-copy=\"Databin[", esc[wlString[context["Databin"]]], "]\">Copy WL</button>; ",
    "each row's Copy WL finds that entry in it, for the publishing account.</p>",
    "<input id=\"filter\" type=\"search\" placeholder=\"Filter: a page, a word, an ID, a source\"><span id=\"shown\"></span>",
    "<table><thead><tr>",
    Map["<th><button type=\"button\" class=\"sort\" title=\"Sort by " <> # <> "\">" <> # <> "<span class=\"arrow\">&#8597;</span></button></th>" &, $listColumns],
    "</tr></thead><tbody>",
    Map[
        Function[entry,
            StringJoin[
                "<tr id=\"", entryAnchor[entry], "\">",
                Map["<td class=\"" <> ToLowerCase[#] <> "\">" <> entryCell[entry, #, context] <> "</td>" &, $listColumns],
                "</tr>"
            ]
        ],
        entries
    ],
    "</tbody></table>", listScript, "</body></html>"
];

(* ==================== the endpoints ==================== *)

(* Each endpoint answers through a function of the requester, the request's headers and its
   arguments, which the deployed APIFunction or Delayed calls with $RequesterWolframID and the
   request. settings: Title, Site, Databin, SetStatus, Status, Attachments, Owner, Readers,
   Domains, Pages *)
teamQ[who_, settings_Association] := admittedQ[who, settings["Owner"], settings["Readers"], settings["Domains"]];

listAnswer[who_, settings_Association] := If[ teamQ[who, settings],
    htmlResponse[listHTML[feedbackEntries[settings["Databin"]], Append[settings, "Statuses" -> feedbackStatuses[settings["Status"]]]], 200],
    deniedPage[who, settings["Title"], settings["Domains"]]
];

listPage[settings_Association] := Delayed[listAnswer[$RequesterWolframID, settings]];

$attachmentTypes = {
    "png" -> "image/png", "jpg" | "jpeg" -> "image/jpeg", "gif" -> "image/gif", "webp" -> "image/webp",
    "pdf" -> "application/pdf", "txt" | "csv" | "wl" | "m" -> "text/plain; charset=utf-8",
    _ -> "application/octet-stream"
};

attachmentResponse[directory_String, name_String] := With[{object = CloudObject[directory <> "/" <> name]},
    If[ ! TrueQ[FileExistsQ[object]],
        plainResponse[404, "No such attachment."],
        HTTPResponse[ByteArray[CloudImport[object, "Byte"]], <|
            "ContentType" -> Replace[ToLowerCase[FileExtension[name]], $attachmentTypes],
            "Headers" -> {"Content-Disposition" -> "inline; filename=\"" <> name <> "\"", "Cache-Control" -> "private, max-age=3600"}
        |>]
    ]
];

(* attachment names are plain file names, never paths *)
attachmentNameQ[name_String] := StringMatchQ[name, RegularExpression["[A-Za-z0-9_][A-Za-z0-9._-]{0,159}"]];
attachmentNameQ[_] := False;

fileAnswer[who_, name_, settings_Association] := Which[
    ! teamQ[who, settings], deniedPage[who, settings["Title"], settings["Domains"]],
    ! attachmentNameQ[name], plainResponse[404, "No such attachment."],
    True, attachmentResponse[settings["Attachments"], name]
];

filePage[settings_Association] := APIFunction[{"f" -> "String"},
    fileAnswer[$RequesterWolframID, #f, settings] &
];

(* whether a request comes from one of the site's pages: its Referer is the page, or the page
   followed by a query, a fragment or a path *)
fromPagesQ[referer_String, pages_List] := AnyTrue[pages,
    Function[page, referer === page || StringStartsQ[referer, (page <> "?") | (page <> "#") | (page <> "/")]]];
fromPagesQ[_, _] := False;

(* the reader's own Wolfram ID, for the panel's email field. Every page under /obj/ shares this
   origin, so only the site's pages get it, and it is never cached *)
meAnswer[who_, referer_, pages_List] := If[StringQ[who] && fromPagesQ[referer, pages], who, Null];

mePage[settings_Association] := Delayed[
    HTTPResponse[
        ExportString[<|"email" -> meAnswer[$RequesterWolframID, Lookup[requestHeaders[], "referer", ""], settings["Pages"]]|>, "RawJSON"],
        <|"ContentType" -> "application/json", "Headers" -> {"Cache-Control" -> "private, no-store", "Vary" -> "Cookie, Referer"}|>
    ]
];

(* A browser sends Sec-Fetch-* headers with every request and a kernel sends none, so refusing them
   keeps any page under /obj/, which shares the reader's session, from changing a status on the
   reader's behalf. *)
browserRequestQ[headers_Association] := AnyTrue[Keys[headers], StringStartsQ[#, "sec-fetch-"] &];

(* every status is checked first: none is stored when one is wrong *)
setStatusAnswer[who_, headers_Association, arguments_Association, settings_Association] := With[
    {updates = statusUpdates[arguments]},
    Which[
        ! teamQ[who, settings], deniedPage[who, settings["Title"], settings["Domains"]],
        browserRequestQ[headers], plainResponse[403, "A status is changed from a Wolfram kernel, with the code a Status button copies."],
        ! MatchQ[updates, {__Association}], plainResponse[400, "Give an id and a status, or updates: a JSON list of objects with id, status and note."],
        ! AllTrue[updates, MemberQ[$statusValues, #["status"]] &], plainResponse[400, "The status is one of " <> StringRiffle[$statusValues, ", "] <> "; nothing was stored."],
        ! MatchQ[storeStatuses[settings["Status"], updates], _CloudObject], plainResponse[500, "The status could not be stored."],
        True, HTTPResponse[
            ExportString[If[StringQ[Lookup[arguments, "updates", None]], updates, First[updates]], "RawJSON"],
            <|"ContentType" -> "application/json", "Headers" -> {"Cache-Control" -> "no-store"}|>
        ]
    ]
];

setStatusPage[settings_Association] := APIFunction[
    {
        "id" -> <|"Interpreter" -> "String", "Required" -> False|>,
        "status" -> <|"Interpreter" -> "String", "Required" -> False|>,
        "note" -> <|"Interpreter" -> "String", "Required" -> False|>,
        "updates" -> <|"Interpreter" -> "String", "Required" -> False|>
    },
    setStatusAnswer[$RequesterWolframID, requestHeaders[], #, settings] &
];

(* ==================== deploying ==================== *)

(* Permissions -> Automatic: the form and Me public, the team's pages for signed-in IDs; any other
   value applies to every endpoint, such as "Private" for a trial run *)
Options[DeployPacletPageFeedback] = {
    "Title" -> Automatic,
    "Domains" -> {},
    "Readers" -> {},
    "Email" -> None,
    "Account" -> Automatic,
    Permissions -> Automatic
};

DeployPacletPageFeedback::email = "`1` is not an email address.";

(* the feedback Databin of directory: created on the first deploy, found through config after *)
feedbackDatabin[directory_String, title_String] := Module[{config = CloudObject[directory <> "/config"], data, bin},
    data = If[TrueQ[FileExistsQ[config]], CloudGet[config], None];
    If[ MatchQ[data, KeyValuePattern["Databin" -> _String]], Return[data["Databin"], Module]];
    bin = CreateDatabin[<|"Name" -> title <> " documentation feedback"|>];
    If[ ! MatchQ[bin, _Databin], Return[$Failed, Module]];
    CloudPut[<|"Databin" -> First[bin]|>, config, Permissions -> "Private"];
    First[bin]
];

DeployPacletPageFeedback[name_, opts : OptionsPattern[]] := Module[
    {directory, site, title, email, bin, settings, public, team, deployed},
    Which[
        ! siteNameQ[name], Message[DeployPacletPage::name, name]; Return[$Failed, Module],
        ! publishingAccountQ[DeployPacletPageFeedback, OptionValue["Account"]], Return[$Failed, Module]
    ];
    site = siteURL[name];
    directory = feedbackDirectory[name];
    title = Replace[OptionValue["Title"], Automatic -> Last[StringSplit[name, "/"]]];
    email = Replace[OptionValue["Email"], None -> ""];
    If[ email =!= "" && ! StringMatchQ[email, RegularExpression["[^\\s@]+@[^\\s@]+"]],
        Message[DeployPacletPageFeedback::email, email]; Return[$Failed, Module]];
    bin = feedbackDatabin[directory, title];
    If[ ! StringQ[bin], Message[DeployPacletPageFeedback::databin, bin]; Return[$Failed, Module]];
    settings = <|
        "Title" -> title,
        "Site" -> site,
        "Databin" -> bin,
        "SetStatus" -> directory <> "/SetStatus",
        "Status" -> directory <> "/status",
        "Attachments" -> directory <> "/attachments",
        "Owner" -> ToLowerCase[connectedAccount[]],
        "Readers" -> ToLowerCase[Flatten[{OptionValue["Readers"]}]],
        "Domains" -> ToLowerCase[StringTrim[Flatten[{OptionValue["Domains"]}], "@"]],
        "Pages" -> {site, site <> "Page"}
    |>;
    {public, team} = Replace[OptionValue[Permissions], {
        Automatic -> {"Public", {"Authenticated" -> {"Read", "Execute"}}},
        permissions_ :> {permissions, permissions}
    }];
    deployed = <|
        "Form" -> CloudDeploy[feedbackForm[bin, settings["Attachments"], title, email], directory <> "/Form", Permissions -> public],
        "All" -> CloudDeploy[listPage[settings], directory <> "/All", Permissions -> team],
        "File" -> CloudDeploy[filePage[settings], directory <> "/File", Permissions -> team],
        "Me" -> CloudDeploy[mePage[settings], directory <> "/Me", Permissions -> public],
        "SetStatus" -> CloudDeploy[setStatusPage[settings], directory <> "/SetStatus", Permissions -> team]
    |>;
    KeyValueMap[
        If[ ! MatchQ[#2, _CloudObject], Message[DeployPacletPageFeedback::deploy, directory <> "/" <> #1, #2]] &,
        deployed
    ];
    If[ ! AllTrue[deployed, MatchQ[#, _CloudObject] &], Return[$Failed, Module]];
    (* the Databin's id stays out of the result, which documentation and logs show; the team's page
       and PacletPageFeedback reach the entries *)
    Join[Map[First, deployed], <|"Team" -> KeyTake[settings, {"Owner", "Readers", "Domains"}]|>]
];

(* ==================== reading and triaging ==================== *)

PacletPageFeedback::none = "The site `1` has no feedback deployed in this account.";

PacletPageFeedback[name_String] := Module[{directory = feedbackDirectory[name], config, statuses},
    config = CloudObject[directory <> "/config"];
    If[ ! TrueQ[$CloudConnected], Message[DeployPacletPage::cloud]; Return[$Failed, Module]];
    If[ ! TrueQ[FileExistsQ[config]], Message[PacletPageFeedback::none, name]; Return[$Failed, Module]];
    statuses = feedbackStatuses[directory <> "/status"];
    Dataset @ Map[
        Function[entry,
            With[{status = Lookup[statuses, TextString[Lookup[entry, "ID", ""]], <||>]},
                Join[entry, <|"Status" -> Lookup[status, "Status", "open"], "Note" -> Lookup[status, "Note", ""]|>]
            ]
        ],
        feedbackEntries[CloudGet[config]["Databin"]]
    ]
];

SetPacletPageFeedbackStatus[name_String, entryID_String, status_String, note_String : ""] := If[
    ! MemberQ[$statusValues, status],
    Message[SetPacletPageFeedbackStatus::status, status, $statusValues]; $Failed,
    URLExecute[CloudObject[feedbackDirectory[name] <> "/SetStatus"], {"id" -> entryID, "status" -> status, "note" -> note}, "RawJSON"]
];

(* several statuses, sent in one request and stored in one write; checked here first, so that
   none is sent when one is wrong *)
SetPacletPageFeedbackStatus[name_String, updates_List] := With[{list = statusUpdate /@ updates},
    Which[
        ! AllTrue[list, AssociationQ], Message[SetPacletPageFeedbackStatus::update, First[Pick[updates, AssociationQ /@ list, False]]]; $Failed,
        ! AllTrue[list, MemberQ[$statusValues, #["status"]] &],
            Message[SetPacletPageFeedbackStatus::status, SelectFirst[list, ! MemberQ[$statusValues, #["status"]] &]["status"], $statusValues]; $Failed,
        (* a request made from the CloudObject carries the kernel's cloud session *)
        True, URLExecute[
            HTTPRequest[CloudObject[feedbackDirectory[name] <> "/SetStatus"], <|"Method" -> "POST", "Body" -> {"updates" -> ExportString[list, "RawJSON", "Compact" -> True]}|>],
            "RawJSON"
        ]
    ]
];

statusUpdate[id_String -> status_String] := <|"id" -> id, "status" -> status, "note" -> ""|>;
statusUpdate[{id_String, status_String}] := <|"id" -> id, "status" -> status, "note" -> ""|>;
statusUpdate[{id_String, status_String, note_String}] := <|"id" -> id, "status" -> status, "note" -> note|>;
statusUpdate[update_Association] /; StringQ[Lookup[update, "ID", None]] && StringQ[Lookup[update, "Status", None]] :=
    <|"id" -> update["ID"], "status" -> update["Status"], "note" -> Replace[Lookup[update, "Note", ""], Except[_String] -> ""]|>;
statusUpdate[_] := $Failed;
