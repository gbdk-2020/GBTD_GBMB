unit Waitbar;
(*****************************************************************)
(*                                                               *)
(* Plaatst Balk op het scherm met melding dat de PC wel bezig is *)
(* maar het even kan duren (om te voorkomen dat de gebruiker in  *)
(* de stress raakt..).                                           *)
(*                                                               *)
(* Werking:                                                      *)
(*                                                               *)
(* ShowWaitBar('Ik ben nu met x bezig');                         *)
(* try                                                           *)
(*   <langdurig process>                                         *)
(* finally                                                       *)
(*   RemoveWaitBar;                                              *)
(* end;                                                          *)
(*                                                               *)
(*****************************************************************)

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, Graphpnl, ExtCtrls, StdCtrls;


type
  TFrmWaitBar = class(TForm)
    GraphicPanel1: TGraphicPanel;
    Shape1: TShape;
    LblWait: TLabel;
    LblInfo: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure ShowWaitBar(const s : string);
procedure RemoveWaitBar;


implementation

uses Oaklib;




{$R *.DFM}

var
  FrmWaitBar: TFrmWaitBar;


procedure ShowWaitBar(const s : string);
(******************************************************)
(* Plaats WachtBalk met tekst s en 'even geduld aub'. *)
(******************************************************)
const BarMsgs : TMsgArray = ('Please wait..', 'Een ogenblik geduld aub..');
begin
  (* Aanmaken *)
  if (FrmWaitBar = nil) then
    FrmWaitBar := TFrmWaitBar.Create(Application);


  with FrmWaitBar do
  begin
    (* info tekst plaatsen *)
    LblInfo.Caption := s;
    LblInfo.Left := (Width - LblInfo.Width) div 2;

    (* 'please wait' in huidige taal *)
    LblWait.Caption := GetLangMsg(BarMsgs);
    LblWait.Left := (Width - LblWait.Width) div 2;
  end;

  (* op scherm *)
  FrmWaitBar.Show;
  FrmWaitBar.Update;
end;



procedure RemoveWaitBar;
(*****************************)
(* Verwijderen van WachtBalk *)
(*****************************)
begin
  if Assigned(FrmWaitBar) then
    FrmWaitBar.Close;
end;


end.
