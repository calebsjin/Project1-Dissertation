!function(t,e){"object"==typeof exports&&"string"!=typeof exports.nodeName?module.exports=t.document?e(t,!0):function(t){if(!t.document)throw new Error("blobjs requires a window with a document");return e(t)}:e(t)}(window||this,function(t,e){"use strict";var n=/^((?!chrome|android).)*safari/i.test(navigator.userAgent);if(t.URL=t.URL||t.webkitURL,t.Blob&&t.URL&&!n)try{return new t.Blob,"function"==typeof define&&define.amd&&define("blobjs",[],function(){return t.Blob}),t.Blob}catch(t){}var c=t.BlobBuilder||t.WebKitBlobBuilder||t.MozBlobBuilder||function(t){var c=function(t){return Object.prototype.toString.call(t).match(/^\[object\s(.*)\]$/)[1]},e=function(){this.data=[]},u=function(t,e,n){this.data=t,this.size=t.length,this.type=e,this.encoding=n},n=e.prototype,o=u.prototype,d=t.FileReaderSync,s=function(t){this.code=this[this.name=t]},i="NOT_FOUND_ERR SECURITY_ERR ABORT_ERR NOT_READABLE_ERR ENCODING_ERR NO_MODIFICATION_ALLOWED_ERR INVALID_STATE_ERR SYNTAX_ERR".split(" "),r=i.length,a=t.URL||t.webkitURL||t,l=a.createObjectURL,f=a.revokeObjectURL,p=a,b=t.btoa,R=t.atob,h=t.ArrayBuffer,g=t.Uint8Array,w=/^[\w-]+:\/*\[?[\w\.:-]+\]?(?::[0-9]+)?/;for(u.fake=o.fake=!0;r--;)s.prototype[i[r]]=r+1;return a.createObjectURL||(p=t.URL=function(t){var e,n=document.createElementNS("http://www.w3.org/1999/xhtml","a");return n.href=t,"origin"in n||("data:"===n.protocol.toLowerCase()?n.origin=null:(e=t.match(w),n.origin=e&&e[1])),n}),p.createObjectURL=function(t){var e,n=t.type;return null===n&&(n="application/octet-stream"),t instanceof u?(e="data:"+n,"base64"===t.encoding?e+";base64,"+t.data:"URI"===t.encoding?e+","+decodeURIComponent(t.data):b?e+";base64,"+b(t.data):e+","+encodeURIComponent(t.data)):l?l.call(a,t):void 0},p.revokeObjectURL=function(t){"data:"!==t.substring(0,5)&&f&&f.call(a,t)},n.append=function(t){var e=this.data;if(g&&(t instanceof h||t instanceof g)){for(var n="",o=new g(t),i=0,r=o.length;i<r;i++)n+=String.fromCharCode(o[i]);e.push(n)}else if("Blob"===c(t)||"File"===c(t)){if(!d)throw new s("NOT_READABLE_ERR");var a=new d;e.push(a.readAsBinaryString(t))}else t instanceof u?"base64"===t.encoding&&R?e.push(R(t.data)):"URI"===t.encoding?e.push(decodeURIComponent(t.data)):"raw"===t.encoding&&e.push(t.data):("string"!=typeof t&&(t+=""),e.push(unescape(encodeURIComponent(t))))},n.getBlob=function(t){return arguments.length||(t=null),new u(this.data.join(""),t,"raw")},n.toString=function(){return"[object BlobBuilder]"},o.slice=function(t,e,n){var o=arguments.length;return o<3&&(n=null),new u(this.data.slice(t,1<o?e:this.data.length),n,this.encoding)},o.toString=function(){return"[object Blob]"},o.close=function(){this.size=0,delete this.data},e}(t),o=function(t,e){var n=e&&e.type||"",o=new c;if(t)for(var i=0,r=t.length;i<r;i++)Uint8Array&&t[i]instanceof Uint8Array?o.append(t[i].buffer):o.append(t[i]);var a=o.getBlob(n);return!a.slice&&a.webkitSlice&&(a.slice=a.webkitSlice),a},i=Object.getPrototypeOf||function(t){return t.__proto__};return o.prototype=i(new t.Blob),"function"==typeof define&&define.amd&&define("blobjs",[],function(){return o}),void 0===e&&(t.Blob=o),o});