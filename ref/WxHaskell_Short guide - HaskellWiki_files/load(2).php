/*
exception 'MWException' with message 'ResourceLoaderFileModule::readScriptFiles: script file not found: "/home/web/wikidata/skin//common/ajax.js"' in /usr/share/mediawiki/includes/resourceloader/ResourceLoaderFileModule.php:552
Stack trace:
#0 /usr/share/mediawiki/includes/resourceloader/ResourceLoaderFileModule.php(258): ResourceLoaderFileModule->readScriptFiles(Array)
#1 /usr/share/mediawiki/includes/resourceloader/ResourceLoader.php(717): ResourceLoaderFileModule->getScript(Object(ResourceLoaderContext))
#2 /usr/share/mediawiki/includes/resourceloader/ResourceLoader.php(491): ResourceLoader->makeModuleResponse(Object(ResourceLoaderContext), Array, Array)
#3 /usr/share/mediawiki/load.php(47): ResourceLoader->respond(Object(ResourceLoaderContext))
#4 {main}
*/
/*
exception 'MWException' with message 'ResourceLoaderFileModule::readScriptFiles: script file not found: "/home/web/wikidata/skin//common/wikibits.js"' in /usr/share/mediawiki/includes/resourceloader/ResourceLoaderFileModule.php:552
Stack trace:
#0 /usr/share/mediawiki/includes/resourceloader/ResourceLoaderFileModule.php(258): ResourceLoaderFileModule->readScriptFiles(Array)
#1 /usr/share/mediawiki/includes/resourceloader/ResourceLoader.php(717): ResourceLoaderFileModule->getScript(Object(ResourceLoaderContext))
#2 /usr/share/mediawiki/includes/resourceloader/ResourceLoader.php(491): ResourceLoader->makeModuleResponse(Object(ResourceLoaderContext), Array, Array)
#3 /usr/share/mediawiki/load.php(47): ResourceLoader->respond(Object(ResourceLoaderContext))
#4 {main}
*/
mw.loader.implement("jquery.client",function($){(function($){var profileCache={};$.client={profile:function(nav){if(nav===undefined){nav=window.navigator;}if(profileCache[nav.userAgent]===undefined){var uk='unknown';var x='x';var wildUserAgents=['Opera','Navigator','Minefield','KHTML','Chrome','PLAYSTATION 3'];var userAgentTranslations=[[/(Firefox|MSIE|KHTML,\slike\sGecko|Konqueror)/,''],['Chrome Safari','Chrome'],['KHTML','Konqueror'],['Minefield','Firefox'],['Navigator','Netscape'],['PLAYSTATION 3','PS3']];var versionPrefixes=['camino','chrome','firefox','netscape','netscape6','opera','version','konqueror','lynx','msie','safari','ps3'];var versionSuffix='(\\/|\\;?\\s|)([a-z0-9\\.\\+]*?)(\\;|dev|rel|\\)|\\s|$)';var names=['camino','chrome','firefox','netscape','konqueror','lynx','msie','opera','safari','ipod','iphone','blackberry','ps3'];var nameTranslations=[];var layouts=['gecko','konqueror','msie','opera','webkit'];var layoutTranslations=[['konqueror','khtml'],['msie','trident'],[
'opera','presto']];var layoutVersions=['applewebkit','gecko'];var platforms=['win','mac','linux','sunos','solaris','iphone'];var platformTranslations=[['sunos','solaris']];var translate=function(source,translations){for(var i=0;i<translations.length;i++){source=source.replace(translations[i][0],translations[i][1]);}return source;};var ua=nav.userAgent,match,name=uk,layout=uk,layoutversion=uk,platform=uk,version=x;if(match=new RegExp('('+wildUserAgents.join('|')+')').exec(ua)){ua=translate(ua,userAgentTranslations);}ua=ua.toLowerCase();if(match=new RegExp('('+names.join('|')+')').exec(ua)){name=translate(match[1],nameTranslations);}if(match=new RegExp('('+layouts.join('|')+')').exec(ua)){layout=translate(match[1],layoutTranslations);}if(match=new RegExp('('+layoutVersions.join('|')+')\\\/(\\d+)').exec(ua)){layoutversion=parseInt(match[2],10);}if(match=new RegExp('('+platforms.join('|')+')').exec(nav.platform.toLowerCase())){platform=translate(match[1],platformTranslations);}if(match=new
RegExp('('+versionPrefixes.join('|')+')'+versionSuffix).exec(ua)){version=match[3];}if(name.match(/safari/)&&version>400){version='2.0';}if(name==='opera'&&version>=9.8){version=ua.match(/version\/([0-9\.]*)/i)[1]||10;}var versionNumber=parseFloat(version,10)||0.0;profileCache[nav.userAgent]={'name':name,'layout':layout,'layoutVersion':layoutversion,'platform':platform,'version':version,'versionBase':(version!==x?Math.floor(versionNumber).toString():x),'versionNumber':versionNumber};}return profileCache[nav.userAgent];},test:function(map,profile){profile=$.isPlainObject(profile)?profile:$.client.profile();var dir=$('body').is('.rtl')?'rtl':'ltr';if(typeof map[dir]!=='object'||typeof map[dir][profile.name]==='undefined'){return true;}var conditions=map[dir][profile.name];for(var i=0;i<conditions.length;i++){var op=conditions[i][0];var val=conditions[i][1];if(val===false){return false;}else if(typeof val=='string'){if(!(eval('profile.version'+op+'"'+val+'"'))){return false;}}else if(
typeof val=='number'){if(!(eval('profile.versionNumber'+op+val))){return false;}}}return true;}};})(jQuery);;},{},{});mw.loader.implement("jquery.cookie",function($){jQuery.cookie=function(d,e,b){if(arguments.length>1&&String(e)!=="[object Object]"){b=jQuery.extend({},b);if(e===null||e===undefined){b.expires=-1}if(typeof b.expires==="number"){var g=b.expires,c=b.expires=new Date();c.setDate(c.getDate()+g)}e=String(e);return(document.cookie=[encodeURIComponent(d),"=",b.raw?e:encodeURIComponent(e),b.expires?"; expires="+b.expires.toUTCString():"",b.path?"; path="+b.path:"",b.domain?"; domain="+b.domain:"",b.secure?"; secure":""].join(""))}b=e||{};var a,f=b.raw?function(h){return h}:decodeURIComponent;return(a=new RegExp("(?:^|; )"+encodeURIComponent(d)+"=([^;]*)").exec(document.cookie))?f(a[1]):null};;},{},{});mw.loader.implement("jquery.messageBox",function($){(function($){$.messageBoxNew=function(options){options=$.extend({'id':'js-messagebox','parent':'body','insert':'prepend'},
options);var $curBox=$('#'+options.id);if($curBox.length>0){if($curBox.hasClass('js-messagebox')){return $curBox;}else{return $curBox.addClass('js-messagebox');}}else{var $newBox=$('<div>',{'id':options.id,'class':'js-messagebox','css':{'display':'none'}});if($(options.parent).length<1){options.parent='body';}if(options.insert==='append'){$newBox.appendTo(options.parent);return $newBox;}else{$newBox.prependTo(options.parent);return $newBox;}}};$.messageBox=function(options){options=$.extend({'message':'','group':'default','replace':false,'target':'js-messagebox'},options);var $target=$.messageBoxNew({id:options.target});var groupID=options.target+'-'+options.group;var $group=$('#'+groupID);if($group.length<1){$group=$('<div>',{'id':groupID,'class':'js-messagebox-group'});$target.prepend($group);}if(options.replace===true){$group.empty();}if(options.message===''||options.message===null){$group.hide();}else{$group.prepend($('<p>').append(options.message)).show();$target.slideDown();}if(
$target.find('> *:visible').length===0){$group.show();$target.slideUp();$group.hide();}else{$target.slideDown();}return $group;};})(jQuery);;},{"all":".js-messagebox{margin:1em 5%;padding:0.5em 2.5%;border:1px solid #ccc;background-color:#fcfcfc;font-size:0.8em}.js-messagebox .js-messagebox-group{margin:1px;padding:0.5em 2.5%;border-bottom:1px solid #ddd}.js-messagebox .js-messagebox-group:last-child{border-bottom:thin none transparent}\n\n/* cache key: wikidb_haskell:resourceloader:filter:minify-css:7:8b08bdc91c52a9ffba396dccfb5b473c */"},{});mw.loader.implement("jquery.mwExtension",function($){(function($){$.extend({trimLeft:function(str){return str===null?'':str.toString().replace(/^\s+/,'');},trimRight:function(str){return str===null?'':str.toString().replace(/\s+$/,'');},ucFirst:function(str){return str.charAt(0).toUpperCase()+str.substr(1);},escapeRE:function(str){return str.replace(/([\\{}()|.?*+\-^$\[\]])/g,"\\$1");},isDomElement:function(el){return!!el&&!!el.nodeType;},isEmpty
:function(v){if(v===''||v===0||v==='0'||v===null||v===false||v===undefined){return true;}if(v.length===0){return true;}if(typeof v==='object'){for(var key in v){return false;}return true;}return false;},compareArray:function(arrThis,arrAgainst){if(arrThis.length!=arrAgainst.length){return false;}for(var i=0;i<arrThis.length;i++){if($.isArray(arrThis[i])){if(!$.compareArray(arrThis[i],arrAgainst[i])){return false;}}else if(arrThis[i]!==arrAgainst[i]){return false;}}return true;},compareObject:function(objectA,objectB){if(typeof objectA==typeof objectB){if(typeof objectA=='object'){if(objectA===objectB){return true;}else{var prop;for(prop in objectA){if(prop in objectB){var type=typeof objectA[prop];if(type==typeof objectB[prop]){switch(type){case'object':if(!$.compareObject(objectA[prop],objectB[prop])){return false;}break;case'function':if(objectA[prop].toString()!==objectB[prop].toString()){return false;}break;default:if(objectA[prop]!==objectB[prop]){return false;}break;}}else{return false
;}}else{return false;}}for(prop in objectB){if(!(prop in objectA)){return false;}}}}}else{return false;}return true;}});})(jQuery);;},{},{});mw.loader.implement("mediawiki.page.startup",function($){(function($){mw.page={};$('html').addClass('client-js').removeClass('client-nojs');$(mw.util.init);})(jQuery);;},{},{});mw.loader.implement("mediawiki.util",function($){(function($,mw){"use strict";var util={init:function(){var profile,$tocTitle,$tocToggleLink,hideTocCookie;$.messageBoxNew({id:'mw-js-message',parent:'#content'});profile=$.client.profile();if(profile.name==='opera'){util.tooltipAccessKeyPrefix='shift-esc-';}else if(profile.name==='chrome'){util.tooltipAccessKeyPrefix=(profile.platform==='mac'?'ctrl-option-':profile.platform==='win'?'alt-shift-':'alt-');}else if(profile.platform!=='win'&&profile.name==='safari'&&profile.layoutVersion>526){util.tooltipAccessKeyPrefix='ctrl-alt-';}else if(!(profile.platform==='win'&&profile.name==='safari')&&(profile.name==='safari'||profile.
platform==='mac'||profile.name==='konqueror')){util.tooltipAccessKeyPrefix='ctrl-';}else if(profile.name==='firefox'&&profile.versionBase>'1'){util.tooltipAccessKeyPrefix='alt-shift-';}if($('#bodyContent').length){util.$content=$('#bodyContent');}else if($('#mw_contentholder').length){util.$content=$('#mw_contentholder');}else if($('#article').length){util.$content=$('#article');}else{util.$content=$('#content');}$tocTitle=$('#toctitle');$tocToggleLink=$('#togglelink');if($('#toc').length&&$tocTitle.length&&!$tocToggleLink.length){hideTocCookie=$.cookie('mw_hidetoc');$tocToggleLink=$('<a href="#" class="internal" id="togglelink"></a>').text(mw.msg('hidetoc')).click(function(e){e.preventDefault();util.toggleToc($(this));});$tocTitle.append($tocToggleLink.wrap('<span class="toctoggle"></span>').parent().prepend('&nbsp;[').append(']&nbsp;'));if(hideTocCookie==='1'){util.toggleToc($tocToggleLink);}}},rawurlencode:function(str){str=String(str);return encodeURIComponent(str).replace(/!/g,
'%21').replace(/'/g,'%27').replace(/\(/g,'%28').replace(/\)/g,'%29').replace(/\*/g,'%2A').replace(/~/g,'%7E');},wikiUrlencode:function(str){return util.rawurlencode(str).replace(/%20/g,'_').replace(/%3A/g,':').replace(/%2F/g,'/');},wikiGetlink:function(str){return mw.config.get('wgArticlePath').replace('$1',util.wikiUrlencode(typeof str==='string'?str:mw.config.get('wgPageName')));},wikiScript:function(str){return mw.config.get('wgScriptPath')+'/'+(str||'index')+mw.config.get('wgScriptExtension');},addCSS:function(text){var s=document.createElement('style');s.type='text/css';s.rel='stylesheet';document.getElementsByTagName('head')[0].appendChild(s);if(s.styleSheet){s.styleSheet.cssText=text;}else{s.appendChild(document.createTextNode(String(text)));}return s.sheet||s;},toggleToc:function($toggleLink,callback){var $tocList=$('#toc ul:first');if($tocList.length){if($tocList.is(':hidden')){$tocList.slideDown('fast',callback);$toggleLink.text(mw.msg('hidetoc'));$('#toc').removeClass(
'tochidden');$.cookie('mw_hidetoc',null,{expires:30,path:'/'});return true;}else{$tocList.slideUp('fast',callback);$toggleLink.text(mw.msg('showtoc'));$('#toc').addClass('tochidden');$.cookie('mw_hidetoc','1',{expires:30,path:'/'});return false;}}else{return null;}},getParamValue:function(param,url){url=url||document.location.href;var re=new RegExp('^[^#]*[&?]'+$.escapeRE(param)+'=([^&#]*)'),m=re.exec(url);if(m&&m.length>1){return decodeURIComponent(m[1].replace(/\+/g,'%20'));}return null;},tooltipAccessKeyPrefix:'alt-',tooltipAccessKeyRegexp:/\[(ctrl-)?(alt-)?(shift-)?(esc-)?(.)\]$/,updateTooltipAccessKeys:function($nodes){if(!$nodes){$nodes=$('#column-one a, #mw-head a, #mw-panel a, #p-logo a, input, label');}else if(!($nodes instanceof $)){$nodes=$($nodes);}$nodes.attr('title',function(i,val){if(val&&util.tooltipAccessKeyRegexp.exec(val)){return val.replace(util.tooltipAccessKeyRegexp,'['+util.tooltipAccessKeyPrefix+'$5]');}return val;});},$content:null,addPortletLink:function(
portlet,href,text,id,tooltip,accesskey,nextnode){var $item,$link,$portlet,$ul;if(arguments.length<3){return null;}$link=$('<a>').attr('href',href).text(text);if(tooltip){$link.attr('title',tooltip);}switch(mw.config.get('skin')){case'standard':case'cologneblue':$('#quickbar').append($link.after('<br/>'));return $link[0];case'nostalgia':$('#searchform').before($link).before(' &#124; ');return $link[0];default:$portlet=$('#'+portlet);if($portlet.length===0){return null;}$ul=$portlet.find('ul');if($ul.length===0){if($portlet.find('div:first').length===0){$portlet.append('<ul></ul>');}else{$portlet.find('div').eq(-1).append('<ul></ul>');}$ul=$portlet.find('ul').eq(0);}if($ul.length===0){return null;}$portlet.removeClass('emptyPortlet');if($portlet.hasClass('vectorTabs')){$item=$link.wrap('<li><span></span></li>').parent().parent();}else{$item=$link.wrap('<li></li>').parent();}if(id){$item.attr('id',id);}if(accesskey){$link.attr('accesskey',accesskey);tooltip+=' ['+accesskey+']';$link.attr(
'title',tooltip);}if(accesskey&&tooltip){util.updateTooltipAccessKeys($link);}if(nextnode&&nextnode.parentNode===$ul[0]){$(nextnode).before($item);}else if(typeof nextnode==='string'&&$ul.find(nextnode).length!==0){$ul.find(nextnode).eq(0).before($item);}else{$ul.append($item);}return $item[0];}},jsMessage:function(message,className){if(!arguments.length||message===''||message===null){$('#mw-js-message').empty().hide();return true;}else{var $messageDiv=$('#mw-js-message');if(!$messageDiv.length){$messageDiv=$('<div id="mw-js-message"></div>');if(util.$content.parent().length){util.$content.parent().prepend($messageDiv);}else{return false;}}if(className){$messageDiv.prop('class','mw-js-message-'+className);}if(typeof message==='object'){$messageDiv.empty();$messageDiv.append(message);}else{$messageDiv.html(message);}$messageDiv.slideDown();return true;}},validateEmail:function(mailtxt){var rfc5322_atext,rfc1034_ldh_str,HTML5_email_regexp;if(mailtxt===''){return null;}rfc5322_atext=
"a-z0-9!#$%&'*+\\-/=?^_`{|}~";rfc1034_ldh_str="a-z0-9\\-";HTML5_email_regexp=new RegExp('^'+'['+rfc5322_atext+'\\.]+'+'@'+'['+rfc1034_ldh_str+']+'+'(?:\\.['+rfc1034_ldh_str+']+)*'+'$','i');return(null!==mailtxt.match(HTML5_email_regexp));},isIPv4Address:function(address,allowBlock){if(typeof address!=='string'){return false;}var block=allowBlock?'(?:\\/(?:3[0-2]|[12]?\\d))?':'',RE_IP_BYTE='(?:25[0-5]|2[0-4][0-9]|1[0-9][0-9]|0?[0-9]?[0-9])',RE_IP_ADD='(?:'+RE_IP_BYTE+'\\.){3}'+RE_IP_BYTE;return address.search(new RegExp('^'+RE_IP_ADD+block+'$'))!==-1;},isIPv6Address:function(address,allowBlock){if(typeof address!=='string'){return false;}var block=allowBlock?'(?:\\/(?:12[0-8]|1[01][0-9]|[1-9]?\\d))?':'',RE_IPV6_ADD='(?:'+':(?::|(?::'+'[0-9A-Fa-f]{1,4}'+'){1,7})'+'|'+'[0-9A-Fa-f]{1,4}'+'(?::'+'[0-9A-Fa-f]{1,4}'+'){0,6}::'+'|'+'[0-9A-Fa-f]{1,4}'+'(?::'+'[0-9A-Fa-f]{1,4}'+'){7}'+')';if(address.search(new RegExp('^'+RE_IPV6_ADD+block+'$'))!==-1){return true;}RE_IPV6_ADD='[0-9A-Fa-f]{1,4}'+
'(?:::?'+'[0-9A-Fa-f]{1,4}'+'){1,6}';return address.search(new RegExp('^'+RE_IPV6_ADD+block+'$'))!==-1&&address.search(/::/)!==-1&&address.search(/::.*::/)===-1;}};mw.util=util;})(jQuery,mediaWiki);;},{},{"showtoc":"show","hidetoc":"hide"});mw.loader.state({"mediawiki.legacy.ajax":"missing","mediawiki.legacy.wikibits":"missing"});

/* cache key: wikidb_haskell:resourceloader:filter:minify-js:7:f850790498818fe7fb22ee8a730ecdba */