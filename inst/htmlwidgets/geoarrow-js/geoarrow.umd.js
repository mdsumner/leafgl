!function(e,t){"object"==typeof exports&&"undefined"!=typeof module?t(exports,require("apache-arrow"),require("@math.gl/polygon"),require("apache-arrow/type"),require("proj4"),require("apache-arrow/data"),require("apache-arrow/vector"),require("apache-arrow/enum"),require("apache-arrow/schema")):"function"==typeof define&&define.amd?define(["exports","apache-arrow","@math.gl/polygon","apache-arrow/type","proj4","apache-arrow/data","apache-arrow/vector","apache-arrow/enum","apache-arrow/schema"],t):t((e="undefined"!=typeof globalThis?globalThis:e||self)["@geoarrow/geoarrow-js"]={},e.arrow,e.polygon,e.type$1,e.proj4,e.data$1,e.vector$1,e._enum,e.schema)}(this,(function(e,t,n,r,i,a,o,u,l){"use strict";function f(e){var t=Object.create(null);return e&&Object.keys(e).forEach((function(n){if("default"!==n){var r=Object.getOwnPropertyDescriptor(e,n);Object.defineProperty(t,n,r.get?r:{enumerable:!0,get:function(){return e[n]}})}})),t.default=e,Object.freeze(t)}var c=f(t);function s(e){return"data"in e?e.getChildAt(0):e.children[0]}function p(e){return"data"in e?e.getChildAt(0):e.children[0]}function d(e){return"data"in e?e.getChildAt(0):e.children[0]}function h(e){return"data"in e?e.getChildAt(0):e.children[0]}var y=Object.freeze({__proto__:null,getLineStringChild:p,getMultiLineStringChild:function(e){return"data"in e?e.getChildAt(0):e.children[0]},getMultiPointChild:function(e){return"data"in e?e.getChildAt(0):e.children[0]},getMultiPolygonChild:h,getPointChild:s,getPolygonChild:d});function g(e,t){const r=e.valueOffsets,i=d(e),a=i.valueOffsets,o=p(i),u=o.type.listSize,l=s(o),f=r[t],c=r[t+1],h=a[f],y=a[c],g=l.values.subarray(h*u,y*u);return new n.Polygon(g,{size:u,isClosed:!0})}function m(e,t){const r=e.valueOffsets,i=d(e),a=i.valueOffsets,o=p(i),u=o.type.listSize,l=s(o),f=r[t],c=r[t+1],h=a[f],y=a[c],g=l.values.subarray(h*u,y*u),m=a[f],T=[];for(let e=f+1;e<c;e++)T.push(a[e]-m);const w=n.earcut(g,T,u);for(let e=0;e<w.length;e++)w[e]+=m;return w}function T(e){return r.DataType.isFixedSizeList(e)?!![2,3,4].includes(e.listSize)&&!!r.DataType.isFloat(e.children[0]):!!r.DataType.isStruct(e)&&(!![2,3,4].includes(e.children.length)&&(!!e.children.every((e=>["x","y","z","m"].includes(e.name)))&&!!e.children.every((e=>r.DataType.isFloat(e)))))}function w(e){return!!r.DataType.isList(e)&&!!T(e.children[0].type)}function O(e){return!!r.DataType.isList(e)&&!!w(e.children[0].type)}function B(e){return!!r.DataType.isList(e)&&!!T(e.children[0].type)}function D(e){return!!r.DataType.isList(e)&&!!w(e.children[0].type)}function C(e){return!!r.DataType.isList(e)&&!!O(e.children[0].type)}var L=Object.freeze({__proto__:null,isLineString:w,isMultiLineString:D,isMultiPoint:B,isMultiPolygon:C,isPoint:T,isPolygon:O});function I(e){return T(e.type)}function b(e){return w(e.type)}function v(e){return O(e.type)}function S(e){return B(e.type)}function E(e){return D(e.type)}function P(e){return C(e.type)}var x,A,Y=Object.freeze({__proto__:null,isLineStringData:b,isMultiLineStringData:E,isMultiPointData:S,isMultiPolygonData:P,isPointData:I,isPolygonData:v});function _(e,t){return I(e)?F(e,t):b(e)?N(e,t):v(e)?V(e,t):S(e)?N(e,t):E(e)?V(e,t):P(e)?function(e,t){const n=h(e),r=V(n,t);return c.makeData({type:e.type,length:e.length,nullCount:e.nullCount,nullBitmap:e.nullBitmap,child:r,valueOffsets:e.valueOffsets})}(e,t):void function(){throw new Error("assertion failed")}()}function F(e,t){!function(e,t){if(!e)throw new Error(`assertion failed ${t}`)}(2===e.type.listSize,"expected 2D");const n=s(e),r=n.values,i=new Float64Array(r.length);for(let n=0;n<e.length;n++){const e=r[2*n],a=r[2*n+1],[o,u]=t(e,a);i[2*n]=o,i[2*n+1]=u}const a=c.makeData({type:n.type,length:n.length,nullCount:n.nullCount,nullBitmap:n.nullBitmap,data:i});return c.makeData({type:e.type,length:e.length,nullCount:e.nullCount,nullBitmap:e.nullBitmap,child:a})}function N(e,t){const n=F(p(e),t);return c.makeData({type:e.type,length:e.length,nullCount:e.nullCount,nullBitmap:e.nullBitmap,child:n,valueOffsets:e.valueOffsets})}function V(e,t){const n=N(d(e),t);return c.makeData({type:e.type,length:e.length,nullCount:e.nullCount,nullBitmap:e.nullBitmap,child:n,valueOffsets:e.valueOffsets})}function M(e,t){const n=[0,0];return _(e,((e,r)=>(n[0]=e,n[1]=r,t.forward(n))))}!function(e){e.POINT="geoarrow.point",e.LINESTRING="geoarrow.linestring",e.POLYGON="geoarrow.polygon",e.MULTIPOINT="geoarrow.multipoint",e.MULTILINESTRING="geoarrow.multilinestring",e.MULTIPOLYGON="geoarrow.multipolygon"}(x||(x={}));class z{minX;minY;maxX;maxY;constructor(){this.minX=1/0,this.minY=1/0,this.maxX=-1/0,this.maxY=-1/0}updateBbox(e){e.minX<this.minX&&(this.minX=e.minX),e.minY<this.minY&&(this.minY=e.minY),e.maxX>this.maxX&&(this.maxX=e.maxX),e.maxY>this.maxY&&(this.maxY=e.maxY)}updateCoord(e,t){e<this.minX&&(this.minX=e),t<this.minY&&(this.minY=t),e>this.maxX&&(this.maxX=e),t>this.maxY&&(this.maxY=t)}}function j(e){const t=s(e).values,n=new z;for(let r=0;r<e.length;r++){const e=t[2*r],i=t[2*r+1];n.updateCoord(e,i)}return n}function U(e){const t=new z;for(const n of e.data)t.updateBbox(j(n));return t}function W(e){return U(p(e))}function X(e){return W(d(e))}!function(e){e[e.CLOCKWISE=n.WINDING.CLOCKWISE]="CLOCKWISE",e[e.COUNTER_CLOCKWISE=n.WINDING.COUNTER_CLOCKWISE]="COUNTER_CLOCKWISE"}(A||(A={}));var k=Object.freeze({__proto__:null,area:function e(t){if("data"in t)return new c.Vector(t.data.map((t=>e(t))));const n=new Float64Array(t.length);for(let e=0;e<t.length;e++){let r=g(t,e);n[e]=r.getArea()}return c.makeData({type:new c.Float(c.Precision.DOUBLE),length:t.length,nullCount:t.nullCount,nullBitmap:t.nullBitmap,data:n})},earcut:function e(t){if("data"in t)return t.data.map((t=>e(t)));const n=[];let r=0;for(let e=0;e<t.length;e++){const i=m(t,e);n.push(i),r+=i.length}const i=new Uint32Array(r);let a=0;for(const e of n)for(const t of e)i[a]=t,a+=1;return i},getMultiPolygonExterior:function e(t){return"data"in t?new c.Vector(t.data.map((t=>e(t)))):h(t)},getPolygonExterior:function e(t){return"data"in t?new c.Vector(t.data.map((t=>e(t)))):d(t)},mapCoords:_,modifyWindingDirection:function e(t,n){if("data"in t)t.data.forEach((t=>e(t,n)));else for(let e=0;e<t.length;e++){g(t,e).modifyWindingDirection(n)}},reproject:function(e,t,n){const r=i(t,n);return"data"in e?new c.Vector(e.data.map((e=>M(e,r)))):M(e,r)},signedArea:function e(t){if("data"in t)return new c.Vector(t.data.map((t=>e(t))));const n=new Float64Array(t.length);for(let e=0;e<t.length;e++){let r=g(t,e);n[e]=r.getSignedArea()}return c.makeData({type:new c.Float(c.Precision.DOUBLE),length:t.length,nullCount:t.nullCount,nullBitmap:t.nullBitmap,data:n})},totalBounds:function(e,t){switch(t.metadata.get("ARROW:extension:name")){case x.POINT:return U(e);case x.LINESTRING:case x.MULTIPOINT:return W(e);case x.POLYGON:case x.MULTILINESTRING:return X(e);case x.MULTIPOLYGON:return function(e){const t=h(e);return X(t)}(e);default:throw new Error("Unknown ext type name")}},windingDirection:function e(t){if("data"in t)return new c.Vector(t.data.map((t=>e(t))));let n=new c.BoolBuilder({type:new c.Bool,nullValues:[null]});n.set(t.length-1,null);for(let e=0;e<t.length;e++){if(!t.getValid(e)){n.setValid(e,!1);continue}let r=g(t,e).getWindingDirection();n.set(e,r===A.CLOCKWISE)}return n.finish().flush()}});var G=Object.freeze({__proto__:null,isLineStringVector:function(e){return w(e.type)},isMultiLineStringVector:function(e){return D(e.type)},isMultiPointVector:function(e){return B(e.type)},isMultiPolygonVector:function(e){return C(e.type)},isPointVector:function(e){return T(e.type)},isPolygonVector:function(e){return O(e.type)}});function R(e,t=!1){if("data"in e)return new o.Vector(e.data.map((e=>R(e,t))));const n=[];for(const r of e.children)n.push(R(r,t));let r;void 0!==e.dictionary&&(r=R(e.dictionary,t));const i={[u.BufferType.OFFSET]:K(e.buffers[u.BufferType.OFFSET],t),[u.BufferType.DATA]:K(e.buffers[u.BufferType.DATA],t),[u.BufferType.VALIDITY]:K(e.buffers[u.BufferType.VALIDITY],t),[u.BufferType.TYPE]:K(e.buffers[u.BufferType.TYPE],t)};return new a.Data(e.type,e.offset,e.length,e._nullCount,i,n,r)}function q(e){return!(0===e.byteOffset&&e.byteLength===e.buffer.byteLength)}function K(e,t){return void 0===e?e:t||q(e)?e.slice():e}function $(e){switch(e.typeId){case u.Type.Null:return new r.Null;case u.Type.Int:return new r.Int(e.isSigned,e.bitWidth);case u.Type.Float:return new r.Float(e.precision);case u.Type.Binary:return new r.Binary;case u.Type.Utf8:return new r.Utf8;case u.Type.Bool:return new r.Bool;case u.Type.Decimal:return new r.Decimal(e.scale,e.precision,e.bitWidth);case u.Type.Date:return new r.Date_(e.unit);case u.Type.Time:return new r.Time(e.unit,e.bitWidth);case u.Type.Timestamp:return new r.Timestamp(e.unit,e.timezone);case u.Type.Interval:return new r.Interval(e.unit);case u.Type.List:{const t=e.children.map(H);if(t.length>1)throw new Error("expected 1 field");return new r.List(t[0])}case u.Type.Struct:{const t=e.children.map(H);return new r.Struct(t)}case u.Type.Union:{const t=e.children.map(H);return new r.Union(e.mode,e.typeIds,t)}case u.Type.FixedSizeBinary:return new r.FixedSizeBinary(e.byteWidth);case u.Type.FixedSizeList:{const t=e.children.map(H);if(t.length>1)throw new Error("expected 1 field");return new r.FixedSizeList(e.listSize,t[0])}case u.Type.Map:{const t=e.children.map(H);if(t.length>1)throw new Error("expected 1 field");const n=t[0];return new r.Map_(n,e.keysSorted)}case u.Type.Duration:return new r.Duration(e.unit);default:throw new Error(`unknown type ${e}`)}}function H(e){const t=$(e.type);return new l.Field(e.name,t,e.nullable,e.metadata)}function J(e){const t=e.children.map((e=>J(e))),n=e.dictionary?Q(e.dictionary):void 0,r={[u.BufferType.OFFSET]:e.valueOffsets,[u.BufferType.DATA]:e.values,[u.BufferType.VALIDITY]:e.nullBitmap,[u.BufferType.TYPE]:e.typeIds};return new a.Data($(e.type),e.offset,e.length,e._nullCount,r,t,n)}function Q(e){return new o.Vector(e.data.map((e=>J(e))))}var Z=Object.freeze({__proto__:null,hardClone:R,isShared:function e(t){if("data"in t)return t.data.some((t=>e(t)));for(const n of t.children)if(e(n))return!0;if(void 0!==t.dictionary&&e(t.dictionary))return!0;const n=[u.BufferType.OFFSET,u.BufferType.DATA,u.BufferType.VALIDITY,u.BufferType.TYPE];for(const e of n)if(void 0!==t.buffers[e]&&q(t.buffers[e]))return!0;return!1},preparePostMessage:function e(t,n=!1){if("data"in t){const n=[],r=[];for(const i of t.data){const[t,a]=e(i);n.push(t),r.push(...a)}return[new o.Vector(n),r]}t=R(t,n);const r=[];for(let n=0;n<t.children.length;n++){const i=t.children[n],[a,o]=e(i);t.children[n]=a,r.push(...o)}if(void 0!==t.dictionary){const[n,i]=e(t.dictionary);t.dictionary=n,r.push(...i)}return void 0!==t.buffers[u.BufferType.OFFSET]&&r.push(t.buffers[u.BufferType.OFFSET].buffer),void 0!==t.buffers[u.BufferType.DATA]&&r.push(t.buffers[u.BufferType.DATA].buffer),void 0!==t.buffers[u.BufferType.VALIDITY]&&r.push(t.buffers[u.BufferType.VALIDITY].buffer),void 0!==t.buffers[u.BufferType.TYPE]&&r.push(t.buffers[u.BufferType.TYPE].buffer),[t,r]},rehydrateData:J,rehydrateVector:Q});e.algorithm=k,e.child=y,e.data=Y,e.type=L,e.vector=G,e.worker=Z}));
//# sourceMappingURL=geoarrow.umd.js.map

