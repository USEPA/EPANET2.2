program Epanet2w;

uses
  WinProcs,
  Forms,
  Fmain in 'Fmain.pas' {MainForm},
  Dabout in 'Dabout.pas' {AboutBoxForm},
  Fbrowser in 'Fbrowser.pas' {BrowserForm},
  Fmap in 'Fmap.pas' {MapForm},
  Fovmap in 'Fovmap.pas' {OVMapForm},
  Umap in 'Umap.pas',
  Ufileio in 'Ufileio.pas',
  Uglobals in 'Uglobals.pas',
  Uimport in 'Uimport.pas',
  Uinput in 'Uinput.pas',
  Uoutput in 'Uoutput.pas',
  Uutils in 'Uutils.pas',
  Dlabel in 'Dlabel.pas' {LabelForm},
  Dpattern in 'Dpattern.pas' {PatternForm},
  Dcurve in 'Dcurve.pas' {CurveForm},
  Dmapdim in 'Dmapdim.pas' {MapDimensionsForm},
  Dcontrol in 'Dcontrol.pas' {ControlsForm},
  Fproped in 'Fproped.pas' {PropEditForm},
  Dmap in 'Dmap.pas' {MapOptionsForm},
  Dlegend in 'Dlegend.pas' {LegendForm},
  Fstatus in 'Fstatus.pas' {StatusForm},
  Fgraph in 'Fgraph.pas' {GraphForm},
  Dfind in 'Dfind.pas' {FindForm},
  Dgrouped in 'Dgrouped.pas' {GroupEditForm},
  Dcalib1 in 'Dcalib1.pas' {CalibDataForm},
  Fcalib in 'Fcalib.pas' {CalibReportForm},
  Ftable in 'Ftable.pas' {TableForm},
  Dcalib2 in 'Dcalib2.pas' {CalibOptionsForm},
  Fsummary in 'Fsummary.pas' {SummaryForm},
  Fcontour in 'Fcontour.pas' {ContourForm},
  Dcontour in 'Dcontour.pas' {ContourOptionsForm},
  Ddataexp in 'Ddataexp.pas' {DataExportForm},
  Dtable in 'Dtable.pas' {TableOptionsForm},
  Udxf in 'Udxf.pas',
  Ddefault in 'Ddefault.pas' {DefaultsForm},
  Dgraph in 'Dgraph.pas' {GraphSelectForm},
  Ddemand in 'Ddemand.pas' {DemandsForm},
  PropEdit in 'PropEdit.pas',
  Fsimul in 'Fsimul.pas' {SimulationForm},
  Fenergy in 'Fenergy.pas' {EnergyForm},
  Dquery in 'Dquery.pas' {QueryForm},
  Dprefers in 'Dprefers.pas' {PreferencesForm},
  Uinifile in 'Uinifile.pas',
  Dcopy in 'Dcopy.pas' {CopyToForm},
  Dinperr in 'Dinperr.pas' {InpErrForm},
  Dmapexp in 'Dmapexp.pas' {MapExportForm},
  epanet2 in 'epanet2.pas',
  Dcolramp in 'Dcolramp.pas' {ColorRampForm},
  Ureport in 'Ureport.pas',
  Uexport in 'Uexport.pas',
  Dchart in 'Dchart.pas' {ChartOptionsDlg},
  Vcl.Themes,
  Vcl.Styles,
  Dsource in 'Dsource.pas' {SourceForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'EPANET 2';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TFindForm, FindForm);
  Application.CreateForm(TQueryForm, QueryForm);
  Application.CreateForm(TOVMapForm, OVMapForm);
  Application.Run;
end.
