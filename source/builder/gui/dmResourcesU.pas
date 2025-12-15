unit dmResourcesU;

interface

uses
  System.SysUtils, System.Classes, System.ImageList, Vcl.ImgList, Vcl.Controls;

type
  TdmResources = class(TDataModule)
    ImageListCommon: TImageList;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmResources: TdmResources;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}
{$R *.dfm}

end.
