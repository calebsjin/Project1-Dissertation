window._colwizStylesPath_={"webpdf/button-chrome.css":"webpdf/button-chrome.4e4b96f2f70b3c71591b0d6782b89af5.css","webpdf/button-web.css":"webpdf/button-web.4e4b96f2f70b3c71591b0d6782b89af5.css","webpdf/csl.css":"webpdf/csl.e5c8e7a9ed200f27898a0737f94f74f3.css","webpdf/popup-chrome.css":"webpdf/popup-chrome.c6b1f466455516ec57830d9b5bb84165.css","webpdf/reader-chrome.css":"webpdf/reader-chrome.9e40f03a691803828c465bbb4d873b20.css","webpdf/reader-desktop.css":"webpdf/reader-desktop.9e40f03a691803828c465bbb4d873b20.css",
"webpdf/reader-web.css":"webpdf/reader-web.9e40f03a691803828c465bbb4d873b20.css"};var CWPDFReaderMode;(function(a){a[a.Chrome=0]="Chrome";a[a.Library=1]="Library";a[a.Publisher=2]="Publisher";a[a.Webimporter=3]="Webimporter";a[a.Drive=4]="Drive";a[a.Firefox=5]="Firefox";a[a.LibraryDesktop=6]="LibraryDesktop";a[a.OpenDrive=7]="OpenDrive"})(CWPDFReaderMode||(CWPDFReaderMode={}));
var CWPDFReaderConfig,CWApiHandler,publisher=window.publisher,publishersList={colwiz:{name:"colwiz",appIds:{stage:null,production:null},logoClass:"cw-logo",pubModeBtnsTxt:{view:"View & annotate PDF",add:"Add to wizdom.ai Library",viewHover:"Read, annotate and save this article using the wizdom.ai Interactive PDF Reader",addHover:"Save this article to your wizdom.ai library to read and reference anywhere"},readerOptions:{sideBarVisible:!1,tools:{dragPage:!0,textSelection:!0,drawHighlight:!0,eraser:!0,
pencil:!0,rectangle:!0,ellipse:!0,arrow:!0,line:!0,comments:!0,download:!0},fullScreenMode:!0,searchBarVisible:!0,addToLibraryShow:!0,referenceTabShow:!1,shareTabShow:!1,citeTabShow:!1,readerSamePage:!0,showClosedAccessmessage:!0},id:"colwiz",buttonStyle:"default",showAddButton:!1}};if("undefined"!==typeof CWPDFReaderConfig)var readerModeTmp=CWPDFReaderConfig.readerMode;
var CWPDFReaderConfigClass=function(){function a(){this.Uid="";this.readerMode="undefined"!==typeof CWApiHandler?CWApiHandler.injectedConfig.readerMode:CWPDFReaderMode.Publisher;this.sentryDomain="www.wizdom.ai";this.importerDomain="webpdf";this.docsPluginEnable=!0;this.debug=!1;this.publicFace="public";this.cwURL="https://app.wizdom.ai";this.webDomain="https://www.wizdom.ai";this.staticDomainURL="https://cdn.wizdom.ai/static";this.assetsDomainURL="https://cdn.wizdom.ai/assets";
this.version="19.04.03-4671-738";this.idURL="https://id.wizdom.ai";this.parser="";this.domain="royalsocietypublishing";this.piwikSiteId=2;this.extensionSpecificFiles={readerSource:"../js/main/04a_pdfreader_chrome.js?1379054322837",readerStyleSource:"../css/"+window._colwizStylesPath_["webpdf/reader-chrome.css"]};this.FirefoxSpecificFiles={readerSource:"resource://jid1-2z9j1M3WgqkBZwww-at-jetpack/colwizwebimporter/data/js/main/04a_pdfreader_firefox.js?1379054322837",readerStyleSource:"resource://jid1-2z9j1M3WgqkBZwww-at-jetpack/colwizwebimporter/data/css/reader-chrome.css?1379054322837"};
this.pubModeBtnsTxt={view:"Read & annotate PDF",add:"Add to wizdom.ai",viewHover:"Read, annotate and save \r\n this article using the wizdom.ai \r\n Interactive PDF Reader",addHover:"Save this article to your \r\n wizdom.ai library to read \r\n and reference anywhere"};this.extModeBtnsTxt={view:"Read & annotate PDF",add:"Add to wizdom.ai"}}a.prototype.isCoreMode=function(){return CWPDFReaderConfig.readerMode===CWPDFReaderMode.Library||CWPDFReaderConfig.readerMode===CWPDFReaderMode.Drive};a.prototype.isDesktopMode=
function(){return CWPDFReaderConfig.readerMode===CWPDFReaderMode.LibraryDesktop||CWPDFReaderConfig.readerMode===CWPDFReaderMode.OpenDrive};a.prototype.checkReaderMode=function(a){var b;CWPDFReaderConfig.readerMode===CWPDFReaderMode.Drive?b="drive":CWPDFReaderConfig.readerMode===CWPDFReaderMode.Library?b="library":CWPDFReaderConfig.readerMode===CWPDFReaderMode.Chrome?b="chrome":CWPDFReaderConfig.readerMode===CWPDFReaderMode.Publisher?b="publisher":CWPDFReaderConfig.readerMode===CWPDFReaderMode.Firefox?
b="firefox":CWPDFReaderConfig.readerMode===CWPDFReaderMode.LibraryDesktop?b="librarydesktop":CWPDFReaderConfig.readerMode===CWPDFReaderMode.OpenDrive&&(b="opendrive");return a?b===a:b};a.prototype.setPublisher=function(a){publisher.appId=a};return a}();CWPDFReaderConfig=new CWPDFReaderConfigClass;"undefined"!==typeof readerModeTmp&&(CWPDFReaderConfig.readerMode=readerModeTmp);
-1!==location.pathname.indexOf("/library/reader")?(CWPDFReaderConfig.readerMode=CWPDFReaderMode.Library,publishersList.colwiz.readerOptions.referenceTabShow=!0):-1!==location.pathname.indexOf("/drive/reader")&&(CWPDFReaderConfig.readerMode=CWPDFReaderMode.Drive);if(CWPDFReaderConfig.isCoreMode()||CWPDFReaderConfig.isDesktopMode())CWPDFReaderConfig.cwURL=location.protocol+"//"+location.host;
if(CWPDFReaderConfig.checkReaderMode("publisher")){var colwizOptions_1;try{if(colwizOptions_1=window.parent.colwizOptions)CWPDFReaderConfig.setPublisher(colwizOptions_1.appId),publisher&&(CWPDFReaderConfig.pubModeBtnsTxt.view=publisher.pubModeBtnsTxt.view,CWPDFReaderConfig.pubModeBtnsTxt.add=publisher.pubModeBtnsTxt.add,CWPDFReaderConfig.pubModeBtnsTxt.viewHover=publisher.pubModeBtnsTxt.viewHover,CWPDFReaderConfig.pubModeBtnsTxt.addHover=publisher.pubModeBtnsTxt.addHover)}catch(e$$6){publisher=publishersList.colwiz}}else publisher=
publishersList.colwiz,CWPDFReaderConfig.isCoreMode()?publisher.readerOptions.addToLibraryShow=!1:CWPDFReaderConfig.isDesktopMode()&&(publisher.readerOptions.readerSamePage=!1,publisher.readerOptions.hideReaderLogo=!0,publisher.readerOptions.hideDownloadButton=!0,publisher.readerOptions.hideReaderAvater=!0,publisher.readerOptions.addToLibraryShow=!1);
var CWMetaData=function(){return function(a){this.authors=this.title=this.refLink=this.pubLink=this.pdfLink=this.identifierType=this.identifier="";this.psp=this.index=0;this.year=this.publisher=this.journal=this.issn=this.type=this.data=this.dataUrl="";this.listingPage=this.openaccess=!1;this.domain=this.otherInfo=this.affiliation=this.keywords="";this.identifier=a.identifier?a.identifier:"";this.identifierType=a.identifierType?a.identifierType:"";this.pdfLink=a.pdfLink?a.pdfLink:"";this.pubLink=
a.pubLink?a.pubLink:"";this.refLink=a.refLink?a.refLink:"";this.title=a.title?a.title:"";this.authors=a.authors?a.authors:"";this.index=a.index?a.index:0;this.psp=a.psp?a.psp:0;this.dataUrl=a.dataUrl?a.dataUrl:"";this.data=a.data?a.data:"";this.type=a.type?a.type:"";this.issn=a.issn?a.issn:"";this.journal=a.journal?a.journal:"";this.publisher=a.publisher?a.publisher:"";this.year=a.year?a.year:"";this.openaccess=a.openaccess?a.openaccess:!1;this.listingPage=a.listingPage?a.listingPage:!1;this.keywords=
a.keywords?a.keywords:"";this.affiliation=a.affiliation?a.affiliation:"";this.otherInfo=a.otherInfo?a.otherInfo:"";this.domain=CWPDFReaderConfig.importerDomain}}(),isMobileDevice=function(){return"undefined"!==typeof window.orientation};
window.isCwMobile={Windows:function(){return/IEMobile/i.test(navigator.userAgent)},Android:function(){return/Android/i.test(navigator.userAgent)},BlackBerry:function(){return/BlackBerry/i.test(navigator.userAgent)},iOS:function(){return/iPhone|iPad|iPod/i.test(navigator.userAgent)},any:function(){return this.Android()||this.BlackBerry()||this.iOS()||this.Windows()}};
var mobileConfigClass=function(){return function(){this.ZOOM_LEVEL=130;this.SCALE=3;this.ASPECT_RATIO=1/this.SCALE;this.MAX_ZOOM=200;this.MIN_ZOOM=120;this.ZOOM_STEP=20}}(),mobileConfig=new mobileConfigClass;
