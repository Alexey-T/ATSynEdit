{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_UpdClient;

interface

type
  TATLineChangeKind = (
    cLineChangeEdited,
    cLineChangeAdded,
    cLineChangeDeleted,
    cLineChangeDeletedAll
    );

type
  IATUpdatableClient = interface
    ['{045EAD6D-5584-4A60-849E-6B8994AA5B8F}']
    procedure Update(AChange: TATLineChangeKind; ALineIndex, ALineCount, ATotalLineCount: integer);
  end;

implementation

end.

