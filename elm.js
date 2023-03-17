!function(t){"use strict";function n(n,r,t){return t.a=n,t.f=r,t}function e(t){return n(2,t,function(r){return function(n){return t(r,n)}})}function b(e){return n(3,e,function(t){return function(r){return function(n){return e(t,r,n)}}})}function c(u){return n(4,u,function(e){return function(t){return function(r){return function(n){return u(e,t,r,n)}}}})}function a(a){return n(5,a,function(u){return function(e){return function(t){return function(r){return function(n){return a(u,e,t,r,n)}}}}})}function W(i){return n(6,i,function(a){return function(u){return function(e){return function(t){return function(r){return function(n){return i(a,u,e,t,r,n)}}}}}})}function C(o){return n(7,o,function(i){return function(a){return function(u){return function(e){return function(t){return function(r){return function(n){return o(i,a,u,e,t,r,n)}}}}}}})}function s(n,r,t){return 2===n.a?n.f(r,t):n(r)(t)}function d(n,r,t,e){return 3===n.a?n.f(r,t,e):n(r)(t)(e)}function l(n,r,t,e,u){return 4===n.a?n.f(r,t,e,u):n(r)(t)(e)(u)}function v(n,r,t,e,u,a){return 5===n.a?n.f(r,t,e,u,a):n(r)(t)(e)(u)(a)}function S(n,r,t,e,u,a,i){return 6===n.a?n.f(r,t,e,u,a,i):n(r)(t)(e)(u)(a)(i)}function F(n,r,t,e,u,a,i,o){return 7===n.a?n.f(r,t,e,u,a,i,o):n(r)(t)(e)(u)(a)(i)(o)}function O(n,r){for(var t,e=[],u=I(n,r,0,e);u&&(t=e.pop());u=I(t.a,t.b,0,e));return u}function I(n,r,t,e){if(n!==r){if("object"!=typeof n||null===n||null===r)return"function"==typeof n&&Z(5),!1;if(100<t)e.push({a:n,b:r});else for(var u in n.$<0&&(n=jr(n),r=jr(r)),n)if(!I(n[u],r[u],t+1,e))return!1}return!0}function $(n,r,t){if("object"!=typeof n)return n===r?0:n<r?-1:1;if(void 0===n.$)return(t=(t=$(n.a,r.a))||$(n.b,r.b))||$(n.c,r.c);for(;n.b&&r.b&&!(t=$(n.a,r.a));n=n.b,r=r.b);return t||(n.b?1:r.b?-1:0)}var P=e(function(n,r){n=$(n,r);return n<0?Ar:n?kr:yr}),X=0;function o(n,r){var t,e={};for(t in n)e[t]=n[t];for(t in r)e[t]=r[t];return e}function J(n,r){if("string"==typeof n)return n+r;if(!n.b)return r;var t={$:1,a:n.a,b:r};n=n.b;for(var e=t;n.b;n=n.b)e=e.b={$:1,a:n.a,b:r};return t}var h={$:0};function U(n,r){return{$:1,a:n,b:r}}var Y=e(U);function g(n){for(var r=h,t=n.length;t--;)r={$:1,a:n[t],b:r};return r}var Q=b(function(n,r,t){for(var e=Array(n),u=0;u<n;u++)e[u]=t(r+u);return e}),r=e(function(n,r){for(var t=Array(n),e=0;e<n&&r.b;e++)t[e]=r.a,r=r.b;return t.length=e,{a:t,b:r}});function Z(n){throw Error("https://github.com/elm/core/blob/1.0.0/hints/"+n+".md")}var G=e(Math.pow),V=e(function(n,r){r%=n;return 0===n?Z(11):0<r&&n<0||r<0&&0<n?r+n:r}),K=Math.cos,nn=Math.sin,rn=e(Math.atan2);var tn=Math.ceil,en=Math.floor,un=Math.sqrt,an=Math.log;function on(n){return n+""}var fn={$:2,b:function(n){return"boolean"==typeof n?_(n):m("a BOOL",n)}},cn={$:2,b:function(n){return"string"==typeof n?_(n):n instanceof String?_(n+""):m("a STRING",n)}};var vn=e(function(n,r){return{$:6,d:n,b:r}});var bn=e(function(n,r){return{$:9,f:n,g:[r]}}),sn=e(p);function p(n,r){switch(n.$){case 2:return n.b(r);case 5:return null===r?_(n.c):m("null",r);case 3:return ln(r)?dn(n.b,r,g):m("a LIST",r);case 4:return ln(r)?dn(n.b,r,$n):m("an ARRAY",r);case 6:var t=n.d;return"object"==typeof r&&null!==r&&t in r?(a=p(n.b,r[t]),z(a)?a:j(s(zr,t,a.a))):m("an OBJECT with a field named `"+t+"`",r);case 7:t=n.e;return ln(r)?t<r.length?(a=p(n.b,r[t]),z(a)?a:j(s(Tr,t,a.a))):m("a LONGER array. Need index "+t+" but only see "+r.length+" entries",r):m("an ARRAY",r);case 8:if("object"!=typeof r||null===r||ln(r))return m("an OBJECT",r);var e,u=h;for(e in r)if(r.hasOwnProperty(e)){var a=p(n.b,r[e]);if(!z(a))return j(s(zr,e,a.a));u={$:1,a:{a:e,b:a.a},b:u}}return _(Rr(u));case 9:for(var i=n.f,o=n.g,f=0;f<o.length;f++){a=p(o[f],r);if(!z(a))return a;i=i(a.a)}return _(i);case 10:a=p(n.b,r);return z(a)?p(n.h(a.a),r):a;case 11:for(var c=h,v=n.g;v.b;v=v.b){a=p(v.a,r);if(z(a))return a;c={$:1,a:a.a,b:c}}return j(qr(Rr(c)));case 1:return j(s(_r,n.a,r));case 0:return _(n.a)}}function dn(n,r,t){for(var e=r.length,u=Array(e),a=0;a<e;a++){var i=p(n,r[a]);if(!z(i))return j(s(Tr,a,i.a));u[a]=i.a}return _(t(u))}function ln(n){return Array.isArray(n)||"undefined"!=typeof FileList&&n instanceof FileList}function $n(r){return s(Qr,r.length,function(n){return r[n]})}function m(n,r){return j(s(_r,"Expecting "+n,r))}function f(n,r){if(n===r)return!0;if(n.$!==r.$)return!1;switch(n.$){case 0:case 1:return n.a===r.a;case 2:return n.b===r.b;case 5:return n.c===r.c;case 3:case 4:case 8:return f(n.b,r.b);case 6:return n.d===r.d&&f(n.b,r.b);case 7:return n.e===r.e&&f(n.b,r.b);case 9:return n.f===r.f&&hn(n.g,r.g);case 10:return n.h===r.h&&f(n.b,r.b);case 11:return hn(n.g,r.g)}}function hn(n,r){var t=n.length;if(t!==r.length)return!1;for(var e=0;e<t;e++)if(!f(n[e],r[e]))return!1;return!0}function gn(n){return n}function pn(n){return{$:0,a:n}}var mn=e(function(n,r){return{$:3,b:n,d:r}});var wn=0;function yn(n){n={$:0,e:wn++,f:n,g:null,h:[]};return zn(n),n}function kn(r){return{$:2,b:function(n){n({$:0,a:yn(r)})},c:null}}function An(n,r){n.h.push(r),zn(n)}var En=e(function(r,t){return{$:2,b:function(n){An(r,t),n({$:0,a:X})},c:null}});var jn=!1,_n=[];function zn(n){if(_n.push(n),!jn){for(jn=!0;n=_n.shift();)!function(r){for(;r.f;){var n=r.f.$;if(0===n||1===n){for(;r.g&&r.g.$!==n;)r.g=r.g.i;if(!r.g)return;r.f=r.g.b(r.f.a),r.g=r.g.i}else{if(2===n)return r.f.c=r.f.b(function(n){r.f=n,zn(r)});if(5===n){if(0===r.h.length)return;r.f=r.f.b(r.h.shift())}else r.g={$:3===n?0:1,b:r.f.b,i:r.g},r.f=r.f.d}}}(n);jn=!1}}function Tn(n,r,t,e,u,a){var n=s(sn,n,r?r.flags:void 0),i=(z(n)||Z(2),{}),r=t(n.a),o=r.a,f=a(c,o),t=function(n,r){var t,e;for(e in w){var u=w[e];u.a&&((t=t||{})[e]=u.a(e,r)),n[e]=function(n,r){var e={g:r,h:void 0},u=n.c,a=n.d,i=n.e,o=n.f;function f(t){return s(mn,f,{$:5,b:function(n){var r=n.a;return 0===n.$?d(a,e,r,t):i&&o?l(u,e,r.i,r.j,t):d(u,e,i?r.i:r.j,t)}})}return e.h=yn(s(mn,f,n.b))}(u,r)}return t}(i,c);function c(n,r){n=s(e,n,o);f(o=n.a,r),xn(i,n.b,u(o))}return xn(i,r.b,u(o)),t?{ports:t}:{}}var w={};var qn=e(function(r,t){return{$:2,b:function(n){r.g(t),n({$:0,a:X})},c:null}}),Dn=e(function(n,r){return s(En,n.h,{$:0,a:r})});function Ln(r){return function(n){return{$:1,k:r,l:n}}}function Nn(n){return{$:2,m:n}}var Rn=[],Mn=!1;function xn(n,r,t){if(Rn.push({p:n,q:r,r:t}),!Mn){Mn=!0;for(var e;e=Rn.shift();)!function(n,r,t){var e,u={};for(e in Bn(!0,r,u,null),Bn(!1,t,u,null),n)An(n[e],{$:"fx",a:u[e]||{i:h,j:h}})}(e.p,e.q,e.r);Mn=!1}}function Bn(n,r,t,e){switch(r.$){case 1:var u=r.k,a=function(n,r,t,e){function u(n){for(var r=t;r;r=r.t)n=r.s(n);return n}return s(n?w[r].e:w[r].f,u,e)}(n,u,e,r.l);return void(t[u]=function(n,r,t){return t=t||{i:h,j:h},n?t.i={$:1,a:r,b:t.i}:t.j={$:1,a:r,b:t.j},t}(n,a,t[u]));case 2:for(var i=r.m;i.b;i=i.b)Bn(n,i.a,t,e);return;case 3:Bn(n,r.o,t,{s:r.n,t:e})}}var Hn;var y="undefined"!=typeof document?document:{};function Wn(n){return{$:0,a:n}}var Cn=e(function(a,i){return e(function(n,r){for(var t=[],e=0;r.b;r=r.b){var u=r.a;e+=u.b||0,t.push(u)}return e+=t.length,{$:1,c:i,d:Jn(n),e:t,f:a,b:e}})}),Sn=Cn(void 0);e(function(a,i){return e(function(n,r){for(var t=[],e=0;r.b;r=r.b){var u=r.a;e+=u.b.b||0,t.push(u)}return e+=t.length,{$:2,c:i,d:Jn(n),e:t,f:a,b:e}})})(void 0);var Fn=e(function(n,r){return{$:"a0",n:n,o:r}}),On=e(function(n,r){return{$:"a2",n:n,o:r}}),k=e(function(n,r){return{$:"a3",n:n,o:r}}),In=b(function(n,r,t){return{$:"a4",n:r,o:{f:n,o:t}}}),Pn=/^\s*j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:/i;var Xn;function Jn(n){for(var r={};n.b;n=n.b){var t,e=n.a,u=e.$,a=e.n,e=e.o;"a2"===u?"className"===a?Un(r,a,e):r[a]=e:(t=r[u]||(r[u]={}),"a3"===u&&"class"===a?Un(t,a,e):t[a]=e)}return r}function Un(n,r,t){var e=n[r];n[r]=e?e+" "+t:t}function A(n,r){var t=n.$;if(5===t)return A(n.k||(n.k=n.m()),r);if(0===t)return y.createTextNode(n.a);if(4===t){for(var e=n.k,u=n.j;4===e.$;)"object"!=typeof u?u=[u,e.j]:u.push(e.j),e=e.k;var a={j:u,p:r};(i=A(e,a)).elm_event_node_ref=a}else if(3===t)Yn(i=n.h(n.g),r,n.d);else{var i=n.f?y.createElementNS(n.f,n.c):y.createElement(n.c);Hn&&"a"==n.c&&i.addEventListener("click",Hn(i)),Yn(i,r,n.d);for(var o=n.e,f=0;f<o.length;f++)i.appendChild(A(1===t?o[f]:o[f].b,r))}return i}function Yn(n,r,t){for(var e in t){var u=t[e];"a1"===e?function(n,r){var t,e=n.style;for(t in r)e[t]=r[t]}(n,u):"a0"===e?function(n,r,t){var e,u=n.elmFs||(n.elmFs={});for(e in t){var a=t[e],i=u[e];if(a){if(i){if(i.q.$===a.$){i.q=a;continue}n.removeEventListener(e,i)}i=function(f,n){function c(n){var r=c.q,t=p(r.a,n);if(z(t)){for(var e,r=Vr(r),t=t.a,u=r?r<3?t.a:t.t:t,a=1==r?t.b:3==r&&t.ad,i=(a&&n.stopPropagation(),(2==r?t.b:3==r&&t._)&&n.preventDefault(),f);e=i.j;){if("function"==typeof e)u=e(u);else for(var o=e.length;o--;)u=e[o](u);i=i.p}i(u,a)}}return c.q=n,c}(r,a),n.addEventListener(e,i,Xn&&{passive:Vr(a)<2}),u[e]=i}else n.removeEventListener(e,i),u[e]=void 0}}(n,r,u):"a3"===e?function(n,r){for(var t in r){var e=r[t];void 0!==e?n.setAttribute(t,e):n.removeAttribute(t)}}(n,u):"a4"===e?function(n,r){for(var t in r){var e=r[t],u=e.f,e=e.o;void 0!==e?n.setAttributeNS(u,t,e):n.removeAttributeNS(u,t)}}(n,u):("value"!==e&&"checked"!==e||n[e]!==u)&&(n[e]=u)}}try{window.addEventListener("t",null,Object.defineProperty({},"passive",{get:function(){Xn=!0}}))}catch(n){}function Qn(n,r){var t=[];return D(n,r,t,0),t}function q(n,r,t,e){r={$:r,r:t,s:e,t:void 0,u:void 0};return n.push(r),r}function D(n,r,t,e){if(n!==r){var u=n.$,a=r.$;if(u!==a){if(1!==u||2!==a)return void q(t,0,e,r);r=function(n){for(var r=n.e,t=r.length,e=Array(t),u=0;u<t;u++)e[u]=r[u].b;return{$:1,c:n.c,d:n.d,e:e,f:n.f,b:n.b}}(r),a=1}switch(a){case 5:for(var i=n.l,o=r.l,f=i.length,c=f===o.length;c&&f--;)c=i[f]===o[f];if(c)return void(r.k=n.k);r.k=r.m();var v=[];return D(n.k,r.k,v,0),void(0<v.length&&q(t,1,e,v));case 4:for(var b=n.j,s=r.j,d=!1,l=n.k;4===l.$;)d=!0,"object"!=typeof b?b=[b,l.j]:b.push(l.j),l=l.k;for(var $=r.k;4===$.$;)d=!0,"object"!=typeof s?s=[s,$.j]:s.push($.j),$=$.k;return d&&b.length!==s.length?void q(t,0,e,r):((d?function(n,r){for(var t=0;t<n.length;t++)if(n[t]!==r[t])return;return 1}(b,s):b===s)||q(t,2,e,s),void D(l,$,t,e+1));case 0:return void(n.a!==r.a&&q(t,3,e,r.a));case 1:return void Zn(n,r,t,e,Vn);case 2:return void Zn(n,r,t,e,Kn);case 3:if(n.h!==r.h)return void q(t,0,e,r);v=Gn(n.d,r.d),v=(v&&q(t,4,e,v),r.i(n.g,r.g));v&&q(t,5,e,v)}}}function Zn(n,r,t,e,u){var a;n.c!==r.c||n.f!==r.f?q(t,0,e,r):((a=Gn(n.d,r.d))&&q(t,4,e,a),u(n,r,t,e))}function Gn(n,r,t){var e,u,a,i,o;for(u in n)"a1"===u||"a0"===u||"a3"===u||"a4"===u?(a=Gn(n[u],r[u]||{},u))&&((e=e||{})[u]=a):u in r?(a=n[u])===(i=r[u])&&"value"!==u&&"checked"!==u||"a0"===t&&function(n,r){return n.$==r.$&&f(n.a,r.a)}(a,i)||((e=e||{})[u]=i):(e=e||{})[u]=t?"a1"===t?"":"a0"===t||"a3"===t?void 0:{f:n[u].f,o:void 0}:"string"==typeof n[u]?"":null;for(o in r)o in n||((e=e||{})[o]=r[o]);return e}function Vn(n,r,t,e){var u=n.e,a=r.e,n=u.length,r=a.length;r<n?q(t,6,e,{v:r,i:n-r}):n<r&&q(t,7,e,{v:n,e:a});for(var i=n<r?n:r,o=0;o<i;o++){var f=u[o];D(f,a[o],t,++e),e+=f.b||0}}function Kn(n,r,t,e){for(var u=[],a={},i=[],o=n.e,f=r.e,c=o.length,v=f.length,b=0,s=0,d=e;b<c&&s<v;){var l=o[b],$=f[s],h=l.a,g=$.a,p=l.b,m=$.b,w=void 0,y=void 0;if(h===g)D(p,m,u,++d),d+=p.b||0,b++,s++;else{var k,A,E,j,_=o[b+1],z=f[s+1];if(_&&(A=_.b,y=g===(k=_.a)),z&&(j=z.b,w=h===(E=z.a)),w&&y)D(p,j,u,++d),rr(a,u,h,m,s,i),d+=p.b||0,tr(a,u,h,A,++d),d+=A.b||0,b+=2,s+=2;else if(w)d++,rr(a,u,g,m,s,i),D(p,j,u,d),d+=p.b||0,b+=1,s+=2;else if(y)tr(a,u,h,p,++d),d+=p.b||0,D(A,m,u,++d),d+=A.b||0,b+=2,s+=1;else{if(!_||k!==E)break;tr(a,u,h,p,++d),rr(a,u,g,m,s,i),d+=p.b||0,D(A,j,u,++d),d+=A.b||0,b+=2,s+=2}}}for(;b<c;){p=(l=o[b]).b;tr(a,u,l.a,p,++d),d+=p.b||0,b++}for(;s<v;){var T=T||[];rr(a,u,($=f[s]).a,$.b,void 0,T),s++}(0<u.length||0<i.length||T)&&q(t,8,e,{w:u,x:i,y:T})}var nr="_elmW6BL";function rr(n,r,t,e,u,a){var i,o=n[t];o?1===o.c?(a.push({r:u,A:o}),o.c=2,D(o.z,e,i=[],o.r),o.r=u,o.s.s={w:i,A:o}):rr(n,r,t+nr,e,u,a):(a.push({r:u,A:o={c:0,z:e,r:u,s:void 0}}),n[t]=o)}function tr(n,r,t,e,u){var a,i=n[t];i?0===i.c?(i.c=2,D(e,i.z,a=[],u),q(r,9,u,{w:a,A:i})):tr(n,r,t+nr,e,u):(a=q(r,9,u,void 0),n[t]={c:1,z:e,r:u,s:a})}function er(n,r,t,e){!function n(r,t,e,u,a,i,o){var f=e[u];var c=f.r;for(;c===a;){var v,b=f.$;if(1===b?er(r,t.k,f.s,o):8===b?(f.t=r,f.u=o,0<(v=f.s.w).length&&n(r,t,v,0,a,i,o)):9===b?(f.t=r,f.u=o,(b=f.s)&&(b.A.s=r,0<(v=b.w).length)&&n(r,t,v,0,a,i,o)):(f.t=r,f.u=o),!(f=e[++u])||(c=f.r)>i)return u}var s=t.$;if(4===s){for(var d=t.k;4===d.$;)d=d.k;return n(r,d,e,u,a+1,i,r.elm_event_node_ref)}var l=t.e;var $=r.childNodes;for(var h=0;h<l.length;h++){var g=1===s?l[h]:l[h].b,p=++a+(g.b||0);if(a<=c&&c<=p&&(u=n($[h],g,e,u,a,p,o),!(f=e[u])||(c=f.r)>i))return u;a=p}return u}(n,r,t,0,0,r.b,e)}function ur(n,r,t,e){return 0===t.length?n:(er(n,r,t,e),ar(n,t))}function ar(n,r){for(var t=0;t<r.length;t++){var e=r[t],u=e.t,e=function(n,r){switch(r.$){case 0:return function(n,r,t){var e=n.parentNode,r=A(r,t);r.elm_event_node_ref||(r.elm_event_node_ref=n.elm_event_node_ref);e&&r!==n&&e.replaceChild(r,n);return r}(n,r.s,r.u);case 4:return Yn(n,r.u,r.s),n;case 3:return n.replaceData(0,n.length,r.s),n;case 1:return ar(n,r.s);case 2:return n.elm_event_node_ref?n.elm_event_node_ref.j=r.s:n.elm_event_node_ref={j:r.s,p:r.u},n;case 6:for(var t=r.s,e=0;e<t.i;e++)n.removeChild(n.childNodes[t.v]);return n;case 7:for(var u=(t=r.s).e,e=t.v,a=n.childNodes[e];e<u.length;e++)n.insertBefore(A(u[e],r.u),a);return n;case 9:var i;return(t=r.s)?(void 0!==(i=t.A).r&&n.parentNode.removeChild(n),i.s=ar(n,t.w)):n.parentNode.removeChild(n),n;case 8:return function(n,r){for(var t=r.s,e=function(n,r){if(n){for(var t=y.createDocumentFragment(),e=0;e<n.length;e++){var u=n[e].A;t.appendChild(2===u.c?u.s:A(u.z,r.u))}return t}}(t.y,r),u=(n=ar(n,t.w),t.x),a=0;a<u.length;a++){var i=u[a],o=i.A,o=2===o.c?o.s:A(o.z,r.u);n.insertBefore(o,n.childNodes[i.r])}e&&n.appendChild(e);return n}(n,r);case 5:return r.s(n);default:Z(10)}}(u,e);u===n&&(n=e)}return n}function ir(n){if(3===n.nodeType)return{$:0,a:n.textContent};if(1!==n.nodeType)return{$:0,a:""};for(var r=h,t=n.attributes,e=t.length;e--;)var u=t[e],r={$:1,a:s(k,u.name,u.value),b:r};for(var a=n.tagName.toLowerCase(),i=h,o=n.childNodes,e=o.length;e--;)i={$:1,a:ir(o[e]),b:i};return d(Sn,a,r,i)}var or=c(function(r,n,t,i){return Tn(n,i,r.aO,r.aW,r.aU,function(t,n){var e=r.aX,u=i.node,a=ir(u);return vr(n,function(n){var n=e(n),r=Qn(a,n);u=ur(u,a,r,t),a=n})})}),fr="undefined"!=typeof cancelAnimationFrame?cancelAnimationFrame:function(n){clearTimeout(n)},cr="undefined"!=typeof requestAnimationFrame?requestAnimationFrame:function(n){return setTimeout(n,1e3/60)};function vr(t,e){e(t);var u=0;function a(){u=1===u?0:(cr(a),e(t),1)}return function(n,r){t=n,r?(e(t),2===u&&(u=1)):(0===u&&cr(a),u=2)}}var br={addEventListener:function(){},removeEventListener:function(){}},sr="undefined"!=typeof document?document:br,dr="undefined"!=typeof window?window:br,lr=b(function(t,e,u){return kn({$:2,b:function(n){function r(n){yn(u(n))}return t.addEventListener(e,r,Xn&&{passive:!0}),function(){t.removeEventListener(e,r)}},c:null})}),$r=e(function(n,r){n=p(n,r);return z(n)?Dr(n.a):Lr});function hr(n){return n}function gr(n){return d(u,rt(E),T(h),n)}function pr(n){return{$:1,a:n}}function mr(n){var r=n.b;return s(ct,1664525*n.a+r>>>0,r)}function wr(n){return((n=277803737*((n=n.a)^n>>>4+(n>>>28)))>>>22^n)>>>0}var yr=1,kr=2,Ar=0,E=Y,Er=b(function(n,r,t){for(;;){if(-2===t.$)return r;var e=t.d,u=n,a=d(n,t.b,t.c,d(Er,n,r,t.e));n=u,r=a,t=e}}),jr=function(n){return d(Er,b(function(n,r,t){return s(E,{a:n,b:r},t)}),h,n)},j=function(n){return{$:1,a:n}},_r=e(function(n,r){return{$:3,a:n,b:r}}),zr=e(function(n,r){return{$:0,a:n,b:r}}),Tr=e(function(n,r){return{$:1,a:n,b:r}}),_=function(n){return{$:0,a:n}},qr=function(n){return{$:2,a:n}},Dr=function(n){return{$:0,a:n}},Lr={$:1},Nr=b(function(n,r,t){for(;;){if(!t.b)return r;var e=t.b,u=n,a=s(n,t.a,r);n=u,r=a,t=e}}),Rr=function(n){return d(Nr,E,h,n)},Mr=c(function(n,r,t,e){return{$:0,a:n,b:r,c:t,d:e}}),xr=[],Br=tn,Hr=e(function(n,r){return an(r)/an(n)}),Wr=Br(s(Hr,2,32)),Cr=l(Mr,0,Wr,xr,xr),Sr=Q,Fr=en,Or=function(n){return n.length},Ir=e(function(n,r){return 0<$(n,r)?n:r}),Pr=r,Xr=e(function(n,r){for(;;){var t=s(Pr,32,n),e=t.b,t=s(E,{$:0,a:t.a},r);if(!e.b)return Rr(t);n=e,r=t}}),Jr=e(function(n,r){for(;;){var t=Br(r/32);if(1===t)return s(Pr,32,n).a;n=s(Xr,n,h),r=t}}),Ur=e(function(n,r){var t,e;return r.a?(e=Fr(s(Hr,32,(t=32*r.a)-1)),n=n?Rr(r.g):r.g,n=s(Jr,n,r.a),l(Mr,Or(r.d)+t,s(Ir,5,e*Wr),n,r.d)):l(Mr,Or(r.d),Wr,xr,r.d)}),Yr=a(function(n,r,t,e,u){for(;;){if(r<0)return s(Ur,!1,{g:e,a:t/32|0,d:u});var a={$:1,a:d(Sr,32,r,n)};n=n,r=r-32,t=t,e=s(E,a,e),u=u}}),Qr=e(function(n,r){var t;return 0<n?v(Yr,r,n-(t=n%32)-32,n,h,d(Sr,t,n-t,r)):Cr}),z=function(n){return!n.$},Zr=bn,Gr=function(n){return{$:0,a:n}},Vr=function(n){switch(n.$){case 0:return 0;case 1:return 1;case 2:return 2;default:return 3}},T=pn,br=T(0),Kr=c(function(n,r,t,e){var u,a,i,o;return e.b?(u=e.a,(e=e.b).b?(a=e.a,(e=e.b).b?(i=e.a,(e=e.b).b?(o=e.b,s(n,u,s(n,a,s(n,i,s(n,e.a,500<t?d(Nr,n,r,Rr(o)):l(Kr,n,r,t+1,o)))))):s(n,u,s(n,a,s(n,i,r)))):s(n,u,s(n,a,r))):s(n,u,r)):r}),u=b(function(n,r,t){return l(Kr,n,r,0,t)}),L=e(function(t,n){return d(u,e(function(n,r){return s(E,t(n),r)}),h,n)}),N=mn,nt=e(function(r,n){return s(N,function(n){return T(r(n))},n)}),rt=b(function(t,n,e){return s(N,function(r){return s(N,function(n){return T(s(t,r,n))},e)},n)}),tt=qn,et=e(function(n,r){return kn(s(N,tt(n),r))}),Y=(w.Task={b:br,c:b(function(n,r,t){return s(nt,function(n){return 0},gr(s(L,et(n),r)))}),d:b(function(n,r,t){return T(0)}),e:e(function(n,r){return s(nt,n,r)}),f:void 0},Ln("Task"),or),R={$:-2},ut=R,at=ut,it=at,ot=e(function(n,r){return{D:r,H:n}}),i=e(function(n,r){return{e:n,f:r}}),Q={R:0,S:tn=s(i,326,256),T:-1,ai:{o:s(ot,128,128),an:"assets/boss.png",c:tn,E:0},W:100,z:h},ft={w:Q,P:0,q:{R:0,ai:{o:s(ot,64,64),an:"assets/tank.png",c:s(i,692,552),E:-135},W:100,X:g([{a:"ArrowUp",b:{$:0,a:{$:3,a:180}}},{a:"ArrowDown",b:{$:0,a:{$:3,a:-180}}},{a:"ArrowLeft",b:{$:0,a:{$:2,a:-160}}},{a:"ArrowRight",b:{$:0,a:{$:2,a:160}}},{a:"w",b:{$:0,a:{$:3,a:180}}},{a:"s",b:{$:0,a:{$:3,a:-180}}},{a:"a",b:{$:0,a:{$:2,a:-160}}},{a:"d",b:{$:0,a:{$:2,a:160}}},{a:" ",b:{$:1}}]),z:h}},ct=e(function(n,r){return{$:0,a:n,b:r}}),en=e(function(u,a){return function(n){var r=mr(n),t=(t=a-u)<0?-t:t,e=wr(r);return{a:(134217728*(67108863&wr(n))+(134217727&e))/9007199254740992*t+u,b:mr(r)}}}),vt=hr;qt=vt;function bt(n){return{a:{J:ft,X:it,Q:0},b:Mt}}function st(n){return{$:4,a:n}}function dt(n){return{$:2,a:n}}function lt(n){return{$:3,a:n}}function $t(n){return{$:0,a:n}}function ht(n){return s(Zr,n,s(Bt,"key",Ht))}function gt(n){return{a:J(n.a?"w_":"d_",n.b),b:n}}function pt(n){return d(Nr,e(function(n,r){return d(Vt,n.a,n.b,r)}),ut,n)}function mt(n){var r=void 0!==y.hidden?{aM:"hidden",aI:"visibilitychange"}:void 0!==y.mozHidden?{aM:"mozHidden",aI:"mozvisibilitychange"}:void 0!==y.msHidden?{aM:"msHidden",aI:"msvisibilitychange"}:void 0!==y.webkitHidden?{aM:"webkitHidden",aI:"webkitvisibilitychange"}:{aM:"hidden",aI:"visibilitychange"};return d(oe,0,r.aI,s(Zr,be(n),s(Bt,"target",s(Bt,r.aM,ve))))}function wt(n){return at}function yt(n){var r,t,e,u,a,i,o,f;return-1===n.$&&-1===n.d.$&&-1===n.e.$?-1!==n.e.d.$||n.e.d.a?(e=(f=n.e).b,u=f.c,a=f.d,f=f.e,v(M,1,n.b,n.c,v(M,0,(r=n.d).b,r.c,r.d,r.e),v(M,0,e,u,a,f))):(e=(t=n.e).b,u=t.c,i=(a=t.d).d,o=a.e,f=t.e,v(M,0,a.b,a.c,v(M,1,n.b,n.c,v(M,0,(r=n.d).b,r.c,r.d,r.e),i),v(M,1,e,u,o,f))):n}function kt(n){var r,t,e,u,a,i,o,f,c;return-1===n.$&&-1===n.d.$&&-1===n.e.$?-1!==n.d.d.$||n.d.d.a?(i=(c=n.e).b,o=c.c,f=c.d,c=c.e,v(M,1,r=n.b,t=n.c,v(M,0,(u=n.d).b,u.c,u.d,u=u.e),v(M,0,i,o,f,c))):(r=n.b,t=n.c,u=(e=n.d).e,i=(a=n.e).b,o=a.c,f=a.d,c=a.e,v(M,0,e.b,e.c,v(M,1,(a=e.d).b,a.c,a.d,a.e),v(M,1,r,t,u,v(M,0,i,o,f,c)))):n}function At(n){var r,t,e,u,a,i;return-1===n.$&&-1===n.d.$?(r=n.a,t=n.b,e=n.c,i=(u=n.d).d,a=n.e,1===u.a?-1!==i.$||i.a?-1===(i=yt(n)).$?(n=i.e,v(x,i.a,i.b,i.c,At(i.d),n)):R:v(M,r,t,e,At(u),a):v(M,r,t,e,At(u),a)):R}function Et(n){var r=s(He,2,32),n=Fr(n*r);return s(Be,r,1664525*n+1013904223)/r}function jt(n){return{e:n.c.e+n.o.H/2,f:n.c.f+n.o.D/2}}function _t(n){function r(n){return H(n)}var t=_u(r(100)),e=Au(r(8)),u=zu(H((a=s(i,(a=n.ai.c).e+(n.ai.o.H-100)/2,a.f-20)).e)),a=Tu(H(a.f));return s(ku,h,g([s(Eu,g([u,a,t,e,ju("fill: #00000000; stroke: #ad3f2c; stroke-width: 2;")]),h),s(Eu,g([u,a,e,_u(r(n.W)),ju("fill: #f0573c")]),h)]))}function zt(n){var r={e:n.c.e+n.o.H/2,f:n.c.f+n.o.D/2},r="rotate("+H(n.E)+(","+H(r.e)+(","+H(r.f)))+")";return s(qu,g([Lu(n.an),zu(H(n.c.e)),Tu(H(n.c.f)),Du(r),ju("image-rendering: pixelated;")]),h)}function Tt(n){var r=s(L,function(n){return zt(n.ai)},n.z);return s(ku,h,s(E,zt(n.ai),r))}var qt,r=s(N,function(n){return T(function(n){var r=mr(s(ct,0,1013904223));return mr(s(ct,r.a+n>>>0,r.b))}(n))},{$:2,b:function(n){n({$:0,a:qt(Date.now())})},c:null}),Dt=e(function(n,r){return n(r)}),Lt=b(function(r,n,t){var e,u;return n.b?(e=n.b,u=(n=s(Dt,n.a,t)).b,s(N,function(n){return d(Lt,r,e,u)},s(tt,r,n.a))):T(t)}),bn=b(function(n,r,t){return T(t)}),Nt=e(function(t,n){var e=n;return function(n){var n=e(n),r=n.b;return{a:t(n.a),b:r}}}),Rt=(w.Random={b:r,c:Lt,d:bn,e:e(function(n,r){return s(Nt,n,r)}),f:void 0},Ln("Random")),Mt=s(e(function(n,r){return Rt(s(Nt,n,r))}),pr,s(en,0,1)),xt=Nn,Bt=vn,Ht=cn,Wt=b(function(n,r,t){return{Z:t,az:r,aB:n}}),Ct=T(d(Wt,h,Lr,0)),St=function(t){return{$:2,b:function(n){var r=t.f;2===r.$&&r.c&&r.c(),t.f=null,n({$:0,a:X})},c:null}},Ft={$:2,b:function(n){n({$:0,a:Date.now()})},c:null},Ot={$:2,b:function(n){var r=cr(function(){n({$:0,a:Date.now()})});return function(){fr(r)}},c:null},It=Dn,Pt=kn,qn=b(function(n,t,r){var e=r.az,r=r.Z,u={a:e,b:t};return 1===u.a.$?u.b.b?s(N,function(r){return s(N,function(n){return T(d(Wt,t,Dr(r),n))},Ft)},Pt(s(N,It(n),Ot))):Ct:u.b.b?T(d(Wt,t,e,r)):s(N,function(n){return Ct},St(u.a.a))}),br=b(function(r,t,n){function e(n){return s(tt,r,n.$?(0,n.a)(t-a):(0,n.a)(vt(t)))}var u=n.aB,a=n.Z;return s(N,function(r){return s(N,function(n){return T(d(Wt,u,Dr(r),t))},gr(s(L,e,u)))},Pt(s(N,It(r),Ot)))}),Xt=b(function(n,r,t){return n(r(t))}),Jt=(w["Browser.AnimationManager"]={b:Ct,c:qn,d:br,e:0,f:e(function(n,r){return r.$?{$:1,a:s(Xt,n,r.a)}:{$:0,a:s(Xt,n,r.a)}})},Ln("Browser.AnimationManager")),Ut=function(n){return Jt({$:1,a:n})},Yt=b(function(n,r,t){return{$:0,a:n,b:r,c:t}}),Qt=e(function(n,r){return{as:r,aB:n}}),or=T(s(Qt,h,ut)),M=a(function(n,r,t,e,u){return{$:-1,a:n,b:r,c:t,d:e,e:u}}),x=a(function(n,r,t,e,u){var a,i,o,f;return-1!==u.$||u.a?-1!==e.$||e.a||-1!==e.d.$||e.d.a?v(M,n,r,t,e,u):(a=e.d,f=e.e,v(M,0,e.b,e.c,v(M,1,a.b,a.c,a.d,a.e),v(M,1,r,t,f,u))):(a=u.b,i=u.c,o=u.d,u=u.e,-1!==e.$||e.a?v(M,n,a,i,v(M,0,r,t,e,o),u):v(M,0,r,t,v(M,1,e.b,e.c,e.d,f=e.e),v(M,1,a,i,o,u)))}),Zt=P,Gt=b(function(n,r,t){if(-2===t.$)return v(M,0,n,r,R,R);var e=t.a,u=t.b,a=t.c,i=t.d,o=t.e;switch(s(Zt,n,u)){case 0:return v(x,e,u,a,d(Gt,n,r,i),o);case 1:return v(M,e,u,r,i,o);default:return v(x,e,u,a,i,d(Gt,n,r,o))}}),Vt=b(function(n,r,t){n=d(Gt,n,r,t);return-1!==n.$||n.a?n:v(M,1,n.b,n.c,n.d,n.e)}),Kt=b(function(n,r,t){for(;;){if(-2===t.$)return r;var e=t.e,u=n,a=d(n,t.b,t.c,d(Kt,n,r,t.d));n=u,r=a,t=e}}),ne=W(function(f,c,v,n,r,t){n=d(Kt,b(function(n,r,t){for(;;){var e=t.a,u=t.b;if(!e.b)return{a:e,b:d(v,n,r,u)};var a=e.a,i=a.a,a=a.b,o=e.b;if(0<=$(i,n))return 0<$(i,n)?{a:e,b:d(v,n,r,u)}:{a:o,b:l(c,i,a,r,u)};n=n,r=r,t={a:o,b:d(f,i,a,u)}}}),{a:jr(n),b:t},r),t=n.a,r=n.b;return d(Nr,e(function(n,r){return d(f,n.a,n.b,r)}),r,t)}),re=e(function(n,r){return{ak:r,ao:n}}),te=b(function(r,t,n){return s(nt,function(n){return{a:t,b:n}},d(lr,n.a?dr:sr,n.b,function(n){return s(It,r,s(re,t,n))}))}),ee=e(function(n,r){return d(Kt,Vt,r,n)}),tn=b(function(u,n,r){var t=b(function(n,r,t){var e=t.c;return{a:t.a,b:t.b,c:s(E,d(te,u,n,r),e)}}),e=b(function(n,r,t){var e=t.b,u=t.c;return{a:s(E,r,t.a),b:e,c:u}}),a=c(function(n,r,t,e){var u=e.c;return{a:e.a,b:d(Vt,n,r,e.b),c:u}}),i=s(L,gt,n),n=S(ne,e,a,t,r.as,pt(i),{a:h,b:ut,c:h}),o=n.b,f=n.c;return s(N,function(n){return T(s(Qt,i,s(ee,o,pt(n))))},s(N,function(n){return gr(f)},gr(s(L,St,n.a))))}),ue=b(function(n,r,t){n=n(r);return n.$?t:s(E,n.a,t)}),ae=e(function(n,r){return d(u,ue(n),h,r)}),ie=(w["Browser.Events"]={b:or,c:tn,d:b(function(n,r,t){var e=r.ao,u=r.ak,r=s(ae,function(n){var r=n.b,r=r.c;return O(n.a,e)?s($r,r,u):Lr},t.aB);return s(N,function(n){return T(t)},gr(s(L,tt(n),r)))}),e:0,f:e(function(n,r){return d(Yt,r.a,r.b,s(Zr,n,r.c))})},Ln("Browser.Events")),oe=b(function(n,r,t){return ie(d(Yt,n,r,t))}),fe=s(oe,0,"keydown"),ce=s(oe,0,"keyup"),ve=fn,be=e(function(n,r){return n(r?1:0)}),se=e(function(n,r){return d(Vt,n,0,r)}),de=e(function(n,r){return s(se,n,r)}),le=e(function(n,r){return o(n,{X:r(n.X)})}),$e=Nn(h),he=C(function(n,r,t,e,u,a,i){if(-1!==a.$||a.a){for(;;){if(-1!==i.$||1!==i.a)break;if(-1!==i.d.$)return kt(r);if(1===i.d.a)return kt(r);break}return r}return v(M,t,a.b,a.c,a.d,v(M,0,e,u,a.e,i))}),ge=e(function(n,r){var t,e,u,a,i,o,f;return-2===r.$?R:(t=r.a,u=r.c,a=r.d,i=r.e,$(n,e=r.b)<0?-1===a.$&&1===a.a?-1!==(o=a.d).$||o.a?-1===(o=yt(r)).$?(f=o.e,v(x,o.a,o.b,o.c,s(ge,n,o.d),f)):R:v(M,t,e,u,s(ge,n,a),i):v(M,t,e,u,s(ge,n,a),i):s(pe,n,F(he,n,r,t,e,u,a,i)))}),pe=e(function(n,r){var t,e,u,a,i;return-1===r.$?(t=r.a,e=r.c,u=r.d,a=r.e,O(n,r=r.b)?-1===(i=function(n){for(;;){if(-1!==n.$||-1!==n.d.$)return n;n=n.d}}(a)).$?v(x,t,i.b,i.c,u,At(a)):R:v(x,t,r,e,u,s(ge,n,a))):R}),me=e(function(n,r){n=s(ge,n,r);return-1!==n.$||n.a?n:v(M,1,n.b,n.c,n.d,n.e)}),we=e(function(n,r){return s(me,n,r)}),ye=e(function(n,r){return s(we,n,r)}),ke=e(function(t,n){return d(u,e(function(n,r){return t(n)?s(E,n,r):r}),h,n)}),B=e(function(n,r){var t=r.c.f,e=n.c.f,u=r.c.e,a=n.c.e,i=e+n.o.D,e=$(e,t+r.o.D)<1&&$(t,i)<1,t=a+n.o.H;return $(a,u+r.o.H)<1&&$(u,t)<1&&e}),Ae=e(function(r,n){return s(ke,function(n){return!s(B,r.ai,n.ai)},n)}),Ee=e(function(n,r){return o(r,{z:s(ke,function(n){return $(n.ab,4.86)<0},s(Ae,n,r.z))})}),je=K,_e=nn,ze=e(function(n,r){r=3.141592653589793*r/180;return{a:n*je(r),b:n*_e(r)}}),Te=b(function(n,r,t){switch(r.$){case 0:var e=o(u=t.c,{f:u.f+r.a*n});return o(t,{c:e});case 1:var e=o(u=t.c,{e:u.e+r.a*n});return o(t,{c:e});case 2:return o(t,{E:t.E+r.a*n});default:var u=s(ze,r.a,t.E);return o(t,{c:e={e:t.c.e+n*u.a,f:t.c.f+n*u.b}})}}),qe=e(function(n,r){for(;;){if(!r.b)return!1;var t=r.b;if(n(r.a))return!0;n=n,r=t}}),De=e(function(n,r){return o(n,{E:r})}),Le=un,Ne=e(function(n,r){var t=r.f-n.f,r=r.e-n.e;return Le(r*r+t*t)}),Re=rn,Me=e(function(n,r){return 180*s(Re,r.f-n.f,r.e-n.e)/3.141592653589793}),xe=b(function(n,r,t){return n*(t-r)+r}),Be=V,He=G,We=e(function(n,r){for(;;){var t=r.ai,e=Et(n),u=Et(e),e=s(i,d(xe,e,80,780-r.ai.o.H-80),d(xe,u,80,640-r.ai.o.D-80));if(0<=$(s(Ne,e,t.c),80))return e;n=u,r=r}}),Ce=c(function(n,r,t,e){var u=e.T<0,r=u?s(We,r,e):e.S,t=s(qe,function(n){return s(B,e.ai,n.ai)},t.z)?e.W-4:e.W,a=e.ai,i=d(Te,n,{$:3,a:140},s(De,a,s(Me,a.c,e.S))),n=140*n;return o(e,{S:r,T:u?s(Ne,r,a.c):e.T-n,ai:i,W:t})}),Se=e(function(n,r){return 0<$(r.R+n,.65)}),Fe=e(function(n,r){return s(Me,jt(r.ai),jt(n.ai))}),Oe=b(function(n,r,t){var e=s(qe,function(n){return s(B,t.ai,n.ai)},r.z)?5.86:t.ab+ +n;return o(t,{ai:d(Te,n,{$:3,a:100},s(De,t.ai,s(Fe,r,t))),ab:e})}),Ie=b(function(n,r,t){var e=s(Se,n,t)?s(E,(e=jt(t.ai),{ai:{o:s(ot,48,48),an:"assets/missile.png",c:e,E:0},ab:0}),t.z):t.z;return o(t,{R:s(Se,n,t)?0:t.R+n,z:s(L,s(Oe,n,r),e)})}),Pe=e(function(n,r){return r.b?d(u,E,r,n):n}),Xe=e(function(n,r){for(;;){if(-2===r.$)return Lr;var t=r.c,e=r.d,u=r.e;switch(s(Zt,n,r.b)){case 0:n=n,r=e;continue;case 1:return Dr(t);default:n=n,r=u;continue}}}),Je=e(function(n,r){return!s(Xe,n,r).$}),Ue=e(function(n,r){return s(Je,n,r)}),Ye=e(function(n,r){return s(Ue,n,r)}),Qe=e(function(r,n){return s(ae,function(n){return s(Ye,n.a,r)?Dr(n.b):Lr},n.X)}),Ze={$:2},Ge=e(function(n,r){return s(L,function(n){return Ze},s(ke,e(function(n,r){return s(B,n.ai,r.ai)})(r),n.z))}),Ve=b(function(n,r,t){return n=g([s(Qe,n,t),s(Ge,r,t)]),d(u,Pe,h,n)}),Ke=b(function(n,r,t){return-1<$(n.R+r,t)}),nu=e(function(n,r){var t=s(ze,4,r);return{ah:!1,ai:d(Te,1,{$:3,a:32},{o:s(ot,16,16),an:"assets/bullet.png",c:n,E:r}),aD:{U:t.a,V:t.b}}}),ru=b(function(n,r,t){switch(r.$){case 0:return o(t,{ai:d(Te,n,r.a,t.ai)});case 1:return d(Ke,t,n,.31)?function(n){return o(n,{R:0,z:s(E,s(nu,jt(n.ai),n.ai.E),n.z)})}(t):t;default:return o(t,{W:t.W-33.4})}}),tu=b(function(n,r,t){return d(Nr,ru(n),t,r)}),eu=c(function(n,r,t,e){return function(n){var r=n.ai,t=r.c.e<0?0:0<$(r.c.e+r.o.H,780)?780-r.o.H:r.c.e,e=r.c.f<0?0:0<$(r.c.f+r.o.D,640)?640-r.o.D:r.c.f,r=o(r,{c:s(i,t,e)});return o(n,{ai:r})}(d(tu,n,d(Ve,r,t,e),e))}),uu=e(function(n,r){var t={o:n,an:"",c:s(i,0,0),E:0};return s(ke,function(n){return!n.ah},s(ke,function(n){return s(B,t,n.ai)},r))}),au=e(function(n,r){return o(r,{ai:d(Te,n,{$:1,a:r.aD.U},d(Te,n,{$:0,a:r.aD.V},r.ai))})}),iu=e(function(n,r){return s(au,200*n,r)}),ou=b(function(n,r,t){return o(s(iu,n,t),{ah:s(qe,function(n){return s(B,n.ai,t.ai)},r.z)||s(B,t.ai,r.ai)})}),fu=b(function(n,r,t){return o(t,{R:t.R+n,z:s(uu,s(ot,780,640),s(L,s(ou,n,r),t.z))})}),cu=c(function(n,r,t,e){var u;return e.P?e:(u=l(eu,n,t,t=d(Ie,n,e.q,e.w),d(fu,n,e.w,e.q)),o(e,{w:l(Ce,n,r,e.q,s(Ee,e.q,t)),P:function(n){return 0<n.w.W?0<n.q.W?0:2:1}(e),q:u}))}),Q=e(function(n,r){switch(n.$){case 0:return{a:o(r,{J:l(cu,n.a/1e3,r.Q,r.X,r.J)}),b:Mt};case 1:return{a:o(r,{Q:n.a}),b:$e};case 2:return{a:s(le,r,de(n.a)),b:$e};case 3:return{a:s(le,r,ye(n.a)),b:$e};case 4:return{a:s(le,r,wt),b:$e};default:return bt()}}),vu={$:5},bu=Sn("button"),su=gn,du=e(function(n,r){return s(On,n,su(r))})("className"),lu=k("class"),$u=Sn("div"),H=on,hu=Sn("h1"),gu=Fn,pu=e(function(n,r){return s(gu,n,{$:0,a:r})}),r=Cn("http://www.w3.org/2000/svg"),mu=r("svg"),wu=Wn,yu=k("viewBox"),ku=r("g"),Au=k("height"),Eu=r("rect"),ju=k("style"),_u=k("width"),zu=k("x"),Tu=k("y"),qu=r("image"),Du=k("transform"),Lu=function(n){return d(In,"http://www.w3.org/1999/xlink","xlink:href",function(n){return Pn.test(n)?"":n}(n))},Nu=e(function(n,r){var t=H(780),e=H(640),u=s(bu,g([du("playAgainButton"),s(pu,"click",Gr(n))]),g([wu("PlayAgain")])),a=s(mu,g([yu("0 0 "+t+" "+e),lu("canvas")]),g([_t(r.q),Tt(r.q),_t(r.w),Tt(r.w)]));return s($u,g([du("gameContainer")]),function(){switch(r.P){case 0:return g([a]);case 1:return g([s($u,g([lu("gameText")]),g([s(hu,g([du("h1EndText")]),g([wu("You Win! :-)")])),u])),a]);default:return g([s($u,g([lu("gameText")]),g([s(hu,g([du("h1EndText")]),g([wu("You Lost :(")])),u])),a])}}())}),bn=Y({aO:bt,aU:function(n){return xt(g([Ut($t),fe(ht(dt)),ce(ht(lt)),mt(st)]))},aW:Q,aX:function(n){return s(Nu,vu,n.J)}});en={Main:{init:bn(Gr(0))(0)}},t.Elm?function n(r,t){for(var e in t)e in r?"init"==e?Z(6):n(r[e],t[e]):r[e]=t[e]}(t.Elm,en):t.Elm=en}(this);