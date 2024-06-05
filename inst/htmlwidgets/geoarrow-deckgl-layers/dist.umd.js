!function(t,e){"object"==typeof exports&&"undefined"!=typeof module?e(exports,require("@deck.gl/core"),require("@deck.gl/layers"),require("@geoarrow/geoarrow-js"),require("apache-arrow"),require("@deck.gl/geo-layers"),require("@deck.gl/aggregation-layers"),require("threads")):"function"==typeof define&&define.amd?define(["exports","@deck.gl/core","@deck.gl/layers","@geoarrow/geoarrow-js","apache-arrow","@deck.gl/geo-layers","@deck.gl/aggregation-layers","threads"],e):e(((t="undefined"!=typeof globalThis?globalThis:t||self)["@geoarrow/deck"]=t["@geoarrow/deck"]||{},t["@geoarrow/deck"]["gl-layers"]={}),t.deck,t.deck,t.geoarrow,t.Arrow,t.deck,t.deck,t.threads)}(this,(function(t,e,r,o,s,i,a,n){"use strict";function l(t){var e=Object.create(null);return t&&Object.keys(t).forEach((function(r){if("default"!==r){var o=Object.getOwnPropertyDescriptor(t,r);Object.defineProperty(e,r,o.get?o:{enumerable:!0,get:function(){return t[r]}})}})),e.default=t,Object.freeze(e)}var c=l(o),d=l(s);function p(t){const{props:r,propName:o,propInput:s,chunkIdx:i,geomCoordOffsets:a}=t;if(void 0!==s)if(s instanceof d.Vector){const t=s.data[i];if(d.DataType.isFixedSizeList(t)){e.assert(1===t.children.length);let s=t.children[0].values;a&&(s=h(s,t.type.listSize,a)),r.data.attributes[o]={value:s,size:t.type.listSize,normalized:!0}}else if(d.DataType.isFloat(t)){let e=t.values;a&&(e=h(e,1,a)),r.data.attributes[o]={value:e,size:1}}}else r[o]="function"==typeof s?(t,e)=>"getPolygonOffset"===o?s(t,e):function(t,e){const{index:r,data:o}=t;let s=r;return void 0!==o.invertedGeomOffsets&&(s=o.invertedGeomOffsets[r]),e({index:s,data:{data:o.data,length:o.length,attributes:o.attributes},target:t.target})}(e,s):s}function h(t,e,r){const o=r[r.length-1],s=new t.constructor(o*e);for(let o=0;o<r.length-1;o++){const i=r[o],a=r[o+1];for(let r=i;r<a;r++)for(let i=0;i<e;i++)s[r*e+i]=t[o*e+i]}return s}function g(t,e){const r=function(t,e,r){const o=t.fields.findIndex((t=>t.name===r||t.metadata.get("ARROW:extension:name")===e));return-1!==o?o:null}(t.schema,e);return null===r?null:t.getChildAt(r)}function u(t){const e=t.valueOffsets,r=c.child.getMultiLineStringChild(t).valueOffsets,o=new Int32Array(e.length);for(let t=0;t<o.length;++t)o[t]=r[e[t]];return o}function f(t){const e=t.valueOffsets,r=c.child.getPolygonChild(t).valueOffsets,o=new Int32Array(e.length);for(let t=0;t<o.length;++t)o[t]=r[e[t]];return o}function y(t){const e=c.child.getMultiPolygonChild(t),r=c.child.getPolygonChild(e),o=t.valueOffsets,s=e.valueOffsets,i=r.valueOffsets,a=new Int32Array(o.length);for(let t=0;t<a.length;++t)a[t]=i[s[o[t]]];return a}function P(t){const e=t[t.length-1],r=new(t.length<Math.pow(2,8)?Uint8Array:t.length<Math.pow(2,16)?Uint16Array:Uint32Array)(e);for(let e=0;e<t.length-1;e++){const o=t[e],s=t[e+1];for(let t=o;t<s;t++)r[t]=e}return r}function L(t,e){const r={},o={};for(const[s,i]of Object.entries(t))e.includes(s)||(s.startsWith("get")?r[s]=i:o[s]=i);return[r,o]}function w({info:t,sourceLayer:e},r){let o=t.index;e.props.data.invertedGeomOffsets&&(o=e.props.data.invertedGeomOffsets[o]);const s=e.props.recordBatchIdx,i=e.props.tableOffsets,a=r.batches[s].get(o);if(null===a)return t;return o+=i[s],{...t,index:o,object:a}}function v(t){return t.reduce(((t,e,r)=>(t[r+1]=t[r]+e.length,t)),new Uint32Array(t.length+1))}function m(t,r){const o=[],s=[];for(const[e,r]of Object.entries(t))e.startsWith("get")&&r instanceof d.Vector&&(o.push(r),e.endsWith("Color")&&s.push(r));!function(t,r){for(const o of r)e.assert(t.batches.length===o.data.length);for(const o of r)for(let r=0;r<t.batches.length;r++)e.assert(t.batches[r].numRows===o.data[r].length)}(r,o);for(const t of s)b(t)}function b(t){e.assert(d.DataType.isFixedSizeList(t.type)),e.assert(3===t.type.listSize||4===t.type.listSize),e.assert(d.DataType.isInt(t.type.children[0])),e.assert(8===t.type.children[0].type.bitWidth)}const{data:O,getSourcePosition:S,getTargetPosition:_,...I}=r.ArcLayer.defaultProps,C={_validate:!0},x={...I,...C};class k extends e.CompositeLayer{static defaultProps=x;static layerName="GeoArrowArcLayer";getPickingInfo(t){return w(t,this.props.data)}renderLayers(){return this._renderLayersPoint()}_renderLayersPoint(){const{data:t,getSourcePosition:s,getTargetPosition:i}=this.props;this.props._validate&&(m(this.props,t),e.assert(c.vector.isPointVector(s)),e.assert(c.vector.isPointVector(i)));const[a,n]=L(this.props,["getSourcePosition","getTargetPosition"]),l=v(t.data),d=[];for(let e=0;e<t.batches.length;e++){const c=s.data[e],h=o.child.getPointChild(c).values,g=i.data[e],u=o.child.getPointChild(g).values,f={...C,...n,recordBatchIdx:e,tableOffsets:l,id:`${this.props.id}-geoarrow-arc-${e}`,data:{data:t.batches[e],length:c.length,attributes:{getSourcePosition:{value:h,size:c.type.listSize},getTargetPosition:{value:u,size:g.type.listSize}}}};for(const[t,r]of Object.entries(a))p({props:f,propName:t,propInput:r,chunkIdx:e});const y=new r.ArcLayer(this.getSubLayerProps(f));d.push(y)}return d}}var T;!function(t){t.POINT="geoarrow.point",t.LINESTRING="geoarrow.linestring",t.POLYGON="geoarrow.polygon",t.MULTIPOINT="geoarrow.multipoint",t.MULTILINESTRING="geoarrow.multilinestring",t.MULTIPOLYGON="geoarrow.multipolygon"}(T||(T={}));const{data:M,getPosition:A,...V}=r.ColumnLayer.defaultProps,N={_validate:!0},G={...V,...N};class z extends e.CompositeLayer{static defaultProps=G;static layerName="GeoArrowColumnLayer";getPickingInfo(t){return w(t,this.props.data)}renderLayers(){const{data:t}=this.props,e=g(t,T.POINT);if(null!==e)return this._renderLayersPoint(e);const r=this.props.getPosition;if(void 0!==r&&c.vector.isPointVector(r))return this._renderLayersPoint(r);throw new Error("getPosition not GeoArrow point")}_renderLayersPoint(t){const{data:o}=this.props;this.props._validate&&(e.assert(c.vector.isPointVector(t)),m(this.props,o));const[s,i]=L(this.props,["getPosition"]),a=v(o.data),n=[];for(let e=0;e<o.batches.length;e++){const l=t.data[e],d=c.child.getPointChild(l).values,h={...N,...i,recordBatchIdx:e,tableOffsets:a,id:`${this.props.id}-geoarrow-column-${e}`,data:{data:o.batches[e],length:l.length,attributes:{getPosition:{value:d,size:l.type.listSize}}}};for(const[t,r]of Object.entries(s))p({props:h,propName:t,propInput:r,chunkIdx:e});const g=new r.ColumnLayer(this.getSubLayerProps(h));n.push(g)}return n}}const{data:E,getHexagon:W,...j}=i.H3HexagonLayer.defaultProps,$={_validate:!0},B={...j,...$};class U extends e.CompositeLayer{static defaultProps=B;static layerName="GeoArrowH3HexagonLayer";getPickingInfo(t){return w(t,this.props.data)}renderLayers(){return this._renderLayersPoint()}_renderLayersPoint(){const{data:t,getHexagon:e}=this.props;this.props._validate&&m(this.props,t);const[r,o]=L(this.props,["getHexagon"]),s=v(t.data),a=[];for(let n=0;n<t.batches.length;n++){const l=e.data[n],c=l.values,d={...$,...o,recordBatchIdx:n,tableOffsets:s,id:`${this.props.id}-geoarrow-arc-${n}`,data:{data:t.batches[n],length:l.length,attributes:{getHexagon:{value:c,size:15}}}};for(const[t,e]of Object.entries(r))p({props:d,propName:t,propInput:e,chunkIdx:n});const h=new i.H3HexagonLayer(this.getSubLayerProps(d));a.push(h)}return a}}const{data:H,getPosition:D,...R}=a.HeatmapLayer.defaultProps,q={_validate:!0},F={...R,...q};class Y extends e.CompositeLayer{static defaultProps=F;static layerName="GeoArrowHeatmapLayer";renderLayers(){const{data:t}=this.props;if(void 0!==this.props.getPosition){const t=this.props.getPosition;if(void 0!==t&&c.vector.isPointVector(t))return this._renderLayersPoint(t);throw new Error("getPosition should pass in an arrow Vector of Point type")}{const e=g(t,T.POINT);if(null!==e)return this._renderLayersPoint(e)}throw new Error("getPosition not GeoArrow point")}_renderLayersPoint(t){const{data:r}=this.props;this.props._validate&&(e.assert(c.vector.isPointVector(t)),m(this.props,r));const[o,s]=L(this.props,["getPosition"]),i=v(r.data),n=[];for(let e=0;e<r.batches.length;e++){const l=t.data[e],d=c.child.getPointChild(l).values,h={...q,...s,recordBatchIdx:e,tableOffsets:i,id:`${this.props.id}-geoarrow-heatmap-${e}`,data:{data:r.batches[e],length:l.length,attributes:{getPosition:{value:d,size:l.type.listSize}}}};for(const[t,r]of Object.entries(o))p({props:h,propName:t,propInput:r,chunkIdx:e});const g=new a.HeatmapLayer(this.getSubLayerProps(h));n.push(g)}return n}}const{data:J,getPath:K,...Q}=r.PathLayer.defaultProps,X={_pathType:"open",_validate:!0},Z={...Q,...X};class tt extends e.CompositeLayer{static defaultProps=Z;static layerName="GeoArrowPathLayer";getPickingInfo(t){return w(t,this.props.data)}renderLayers(){const{data:t}=this.props;if(void 0!==this.props.getPath){const t=this.props.getPath;if(void 0!==t&&c.vector.isLineStringVector(t))return this._renderLayersLineString(t);if(void 0!==t&&c.vector.isMultiLineStringVector(t))return this._renderLayersMultiLineString(t);throw new Error("getPath should be an arrow Vector of LineString or MultiLineString type")}{const e=g(t,T.LINESTRING);if(null!==e)return this._renderLayersLineString(e);const r=g(t,T.MULTILINESTRING);if(null!==r)return this._renderLayersMultiLineString(r)}throw new Error("getPath not GeoArrow LineString or MultiLineString")}_renderLayersLineString(t){const{data:o}=this.props;this.props._validate&&(e.assert(c.vector.isLineStringVector(t)),m(this.props,o));const[s,i]=L(this.props,["getPath"]),a=v(o.data),n=[];for(let e=0;e<o.batches.length;e++){const l=t.data[e],d=l.valueOffsets,h=c.child.getLineStringChild(l),g=h.type.listSize,u=c.child.getPointChild(h).values,f={...X,...i,recordBatchIdx:e,tableOffsets:a,id:`${this.props.id}-geoarrow-path-${e}`,data:{data:o.batches[e],length:l.length,startIndices:d,attributes:{getPath:{value:u,size:g}}}};for(const[t,r]of Object.entries(s))p({props:f,propName:t,propInput:r,chunkIdx:e,geomCoordOffsets:d});const y=new r.PathLayer(this.getSubLayerProps(f));n.push(y)}return n}_renderLayersMultiLineString(t){const{data:o}=this.props;this.props._validate&&(e.assert(c.vector.isMultiLineStringVector(t)),m(this.props,o));const[s,i]=L(this.props,["getPath"]),a=v(o.data),n=[];for(let e=0;e<o.batches.length;e++){const l=t.data[e],d=c.child.getMultiLineStringChild(l),h=c.child.getLineStringChild(d),g=c.child.getPointChild(h),f=l.valueOffsets,y=d.valueOffsets,L=h.type.listSize,w=g.values,v=u(l),m={...X,...i,recordBatchIdx:e,tableOffsets:a,id:`${this.props.id}-geoarrow-path-${e}`,data:{data:o.batches[e],invertedGeomOffsets:P(f),length:d.length,startIndices:y,attributes:{getPath:{value:w,size:L}}}};for(const[t,r]of Object.entries(s))p({props:m,propName:t,propInput:r,chunkIdx:e,geomCoordOffsets:v});const b=new r.PathLayer(this.getSubLayerProps(m));n.push(b)}return n}}const{data:et,getPosition:rt,...ot}=r.PointCloudLayer.defaultProps,st={_validate:!0},it={...ot,...st};class at extends e.CompositeLayer{static defaultProps=it;static layerName="GeoArrowPointCloudLayer";getPickingInfo(t){return w(t,this.props.data)}renderLayers(){const{data:t}=this.props;if(void 0!==this.props.getPosition){const t=this.props.getPosition;if(void 0!==t&&c.vector.isPointVector(t))return this._renderLayersPoint(t);throw new Error("getPosition should pass in an arrow Vector of Point type")}{const e=g(t,T.POINT);if(null!==e)return this._renderLayersPoint(e)}throw new Error("getPosition not GeoArrow point")}_renderLayersPoint(t){const{data:o}=this.props;this.props._validate&&(e.assert(c.vector.isPointVector(t),"The geometry column is not a valid PointVector."),e.assert(3===t.type.listSize,"Points of a PointCloudLayer in the geometry column must be three-dimensional."),m(this.props,o));const[s,i]=L(this.props,["getPosition"]),a=v(o.data),n=[];for(let e=0;e<o.batches.length;e++){const l=t.data[e],d=c.child.getPointChild(l).values,h={...st,...i,recordBatchIdx:e,tableOffsets:a,id:`${this.props.id}-geoarrow-pointcloud-${e}`,data:{data:o.batches[e],length:l.length,attributes:{getPosition:{value:d,size:l.type.listSize}}}};for(const[t,r]of Object.entries(s))p({props:h,propName:t,propInput:r,chunkIdx:e});const g=new r.PointCloudLayer(this.getSubLayerProps(h));n.push(g)}return n}}const{data:nt,getPolygon:lt,...ct}=r.SolidPolygonLayer.defaultProps,dt={_normalize:!1,_windingOrder:"CCW",_validate:!0,earcutWorkerUrl:"https://cdn.jsdelivr.net/npm/@geoarrow/geoarrow-js@0.3.0-beta.1/dist/earcut-worker.min.js",earcutWorkerPoolSize:8},pt={...ct,...dt};class ht extends e.CompositeLayer{static defaultProps=pt;static layerName="GeoArrowSolidPolygonLayer";initializeState(t){this.state={table:null,tableOffsets:null,triangles:null,earcutWorkerRequest:null===this.props.earcutWorkerUrl||void 0===this.props.earcutWorkerUrl?null:fetch(this.props.earcutWorkerUrl).then((t=>t.text())),earcutWorkerPool:null}}async initEarcutPool(){if(this.state.earcutWorkerPool)return this.state.earcutWorkerPool;const t=await this.state.earcutWorkerRequest;if(!t)return null;if(window?.location?.href.startsWith("file://"))return null;try{const e=n.Pool((()=>n.spawn(n.BlobWorker.fromText(t))),8);return this.state.earcutWorkerPool=e,this.state.earcutWorkerPool}catch(t){return null}}async finalizeState(t){await(this.state?.earcutWorkerPool?.terminate()),console.log("terminated")}async updateData(){const{data:t}=this.props,e=await this._updateEarcut(t),r=v(t.data);this.setState({table:this.props.data,triangles:e,tableOffsets:r})}async _updateEarcut(t){const e=g(t,T.POLYGON);if(null!==e)return this._earcutPolygonVector(e);const r=g(t,T.MULTIPOLYGON);if(null!==r)return this._earcutMultiPolygonVector(r);const o=this.props.getPolygon;if(void 0!==o&&c.vector.isPolygonVector(o))return this._earcutPolygonVector(o);if(void 0!==o&&c.vector.isMultiPolygonVector(o))return this._earcutMultiPolygonVector(o);throw new Error("geometryColumn not Polygon or MultiPolygon")}async _earcutPolygonVector(t){const e=await this.initEarcutPool();if(!e)return this._earcutPolygonVectorMainThread(t);const r=new Array(t.data.length);console.time("earcut");for(let o=0;o<t.data.length;o++){const s=t.data[o],[i,a]=c.worker.preparePostMessage(s,!0);e.queue((async t=>{const e=await t(n.Transfer(i,a));r[o]=e}))}return await e.completed(),console.timeEnd("earcut"),r}_earcutPolygonVectorMainThread(t){const e=new Array(t.data.length);for(let r=0;r<t.data.length;r++){const o=t.data[r];e[r]=c.algorithm.earcut(o)}return e}async _earcutMultiPolygonVector(t){const e=await this.initEarcutPool();if(!e)return this._earcutMultiPolygonVectorMainThread(t);const r=new Array(t.data.length);console.time("earcut");for(let o=0;o<t.data.length;o++){const s=t.data[o],i=c.child.getMultiPolygonChild(s),[a,l]=c.worker.preparePostMessage(i,!0);e.queue((async t=>{const e=await t(n.Transfer(a,l));r[o]=e}))}return await e.completed(),console.timeEnd("earcut"),r}_earcutMultiPolygonVectorMainThread(t){const e=new Array(t.data.length);for(let r=0;r<t.data.length;r++){const o=t.data[r],s=c.child.getMultiPolygonChild(o);e[r]=c.algorithm.earcut(s)}return e}updateState({props:t,changeFlags:e}){e.dataChanged&&this.updateData()}getPickingInfo(t){return w(t,this.props.data)}renderLayers(){const{table:t}=this.state;if(!t)return null;if(void 0!==this.props.getPolygon){const t=this.props.getPolygon;if(void 0!==t&&c.vector.isPolygonVector(t))return this._renderLayersPolygon(t);if(void 0!==t&&c.vector.isMultiPolygonVector(t))return this._renderLayersMultiPolygon(t);throw new Error("getPolygon should be an arrow Vector of Polygon or MultiPolygon type")}{const e=g(t,T.POLYGON);if(null!==e)return this._renderLayersPolygon(e);const r=g(t,T.MULTIPOLYGON);if(null!==r)return this._renderLayersMultiPolygon(r)}throw new Error("getPolygon not GeoArrow Polygon or MultiPolygon")}_renderLayersPolygon(t){const{table:o}=this.state;if(!o)return null;this.props._validate&&(e.assert(c.vector.isPolygonVector(t)),m(this.props,o));const[s,i]=L(this.props,["getPolygon"]),a=[];for(let e=0;e<o.batches.length;e++){const n=t.data[e],l=c.child.getPolygonChild(n),d=c.child.getLineStringChild(l),h=c.child.getPointChild(d),g=d.type.listSize,u=h.values,y=f(n);if(!this.state.triangles)return null;const P=this.state.triangles[e],L={...dt,...i,recordBatchIdx:e,tableOffsets:this.state.tableOffsets,id:`${this.props.id}-geoarrow-point-${e}`,data:{data:o.batches[e],length:n.length,startIndices:y,attributes:{getPolygon:{value:u,size:g},indices:{value:P,size:1}}}};for(const[t,r]of Object.entries(s))p({props:L,propName:t,propInput:r,chunkIdx:e,geomCoordOffsets:y});const w=new r.SolidPolygonLayer(this.getSubLayerProps(L));a.push(w)}return a}_renderLayersMultiPolygon(t){const{table:o}=this.state;if(!o)return null;this.props._validate&&(e.assert(c.vector.isMultiPolygonVector(t)),m(this.props,o));const[s,i]=L(this.props,["getPolygon"]),a=[];for(let e=0;e<o.batches.length;e++){const n=t.data[e],l=c.child.getMultiPolygonChild(n),d=c.child.getPolygonChild(l),h=c.child.getLineStringChild(d),g=c.child.getPointChild(h),u=h.type.listSize,L=n.valueOffsets,w=g.values;if(!this.state.triangles)return null;const v=this.state.triangles[e],m=f(l),b=y(n),O={...dt,...i,recordBatchIdx:e,tableOffsets:this.state.tableOffsets,id:`${this.props.id}-geoarrow-solid-polygon-multi-${e}`,data:{data:o.batches[e],invertedGeomOffsets:P(L),length:l.length,startIndices:m,attributes:{getPolygon:{value:w,size:u},indices:{value:v,size:1}}}};for(const[t,r]of Object.entries(s))p({props:O,propName:t,propInput:r,chunkIdx:e,geomCoordOffsets:b});const S=this.getSubLayerProps(O),_=new r.SolidPolygonLayer(S);a.push(_)}return a}}function gt(t){return"data"in t?new d.Vector(t.data.map((t=>gt(t)))):t}function ut(t){if("data"in t)return new d.Vector(t.data.map((t=>ut(t))));const e=t.valueOffsets,r=c.child.getMultiPolygonChild(t),o=r.valueOffsets,s=c.child.getPolygonChild(r),i=new Int32Array(e.length);for(let t=0;t<i.length;++t)i[t]=o[e[t]];return d.makeData({type:new d.List(r.type.children[0]),length:t.length,nullCount:t.nullCount,nullBitmap:t.nullBitmap,child:s,valueOffsets:i})}const{data:ft,getPolygon:yt,...Pt}=r.PolygonLayer.defaultProps,Lt={...Pt,_normalize:!1,_windingOrder:"CCW",_validate:!0},wt=[0,0,0,255];class vt extends e.CompositeLayer{static defaultProps=Lt;static layerName="GeoArrowPolygonLayer";getPickingInfo(t){return t.info}renderLayers(){const{data:t}=this.props;if(void 0!==this.props.getPolygon){const t=this.props.getPolygon;if(c.vector.isPolygonVector(t))return this._renderLayers(t);if(c.vector.isMultiPolygonVector(t))return this._renderLayers(t);throw new Error("getPolygon should be an arrow Vector of Polygon or MultiPolygon type")}{const e=g(t,T.POLYGON);if(null!==e)return this._renderLayers(e);const r=g(t,T.MULTIPOLYGON);if(null!==r)return this._renderLayers(r)}throw new Error("geometryColumn not Polygon or MultiPolygon")}_renderLayers(t){const{data:r}=this.props;let o;c.vector.isPolygonVector(t)?o=gt(t):c.vector.isMultiPolygonVector(t)?o=ut(t):e.assert(!1);const{data:s,_dataDiff:i,stroked:a,filled:n,extruded:l,wireframe:d,_normalize:p,_windingOrder:h,elevationScale:g,transitions:u,positionFormat:f}=this.props,{lineWidthUnits:y,lineWidthScale:P,lineWidthMinPixels:L,lineWidthMaxPixels:w,lineJointRounded:v,lineMiterLimit:m,lineDashJustified:b}=this.props,{getFillColor:O,getLineColor:S,getLineWidth:_,getElevation:I,getPolygon:C,updateTriggers:x,material:k}=this.props,T=this.getSubLayerClass("fill",ht),M=this.getSubLayerClass("stroke",tt),A=new T({extruded:l,elevationScale:g,filled:n,wireframe:d,_normalize:p,_windingOrder:h,getElevation:I,getFillColor:O,getLineColor:l&&d?S:wt,material:k,transitions:u},this.getSubLayerProps({id:"fill",updateTriggers:x&&{getPolygon:x.getPolygon,getElevation:x.getElevation,getFillColor:x.getFillColor,getLineColor:x.getLineColor}}),{data:s,positionFormat:f,getPolygon:C});return[!l&&A,!l&&a&&new M({widthUnits:y,widthScale:P,widthMinPixels:L,widthMaxPixels:w,jointRounded:v,miterLimit:m,dashJustified:b,_pathType:"loop",transitions:u&&{getWidth:u.getLineWidth,getColor:u.getLineColor,getPath:u.getPolygon},getColor:this.getSubLayerAccessor(S),getWidth:this.getSubLayerAccessor(_)},this.getSubLayerProps({id:"stroke",updateTriggers:x&&{getWidth:x.getLineWidth,getColor:x.getLineColor,getDashArray:x.getLineDashArray}}),{data:r,positionFormat:f,getPath:o,pickable:!1}),l&&A]}}const{data:mt,getPosition:bt,...Ot}=r.ScatterplotLayer.defaultProps,St={_validate:!0},_t={...Ot,...St};class It extends e.CompositeLayer{static defaultProps=_t;static layerName="GeoArrowScatterplotLayer";getPickingInfo(t){return w(t,this.props.data)}renderLayers(){const{data:t}=this.props;if(void 0!==this.props.getPosition){const t=this.props.getPosition;if(void 0!==t&&c.vector.isPointVector(t))return this._renderLayersPoint(t);if(void 0!==t&&c.vector.isMultiPointVector(t))return this._renderLayersMultiPoint(t);throw new Error("getPosition should pass in an arrow Vector of Point or MultiPoint type")}{const e=g(t,T.POINT);if(null!==e)return this._renderLayersPoint(e);const r=g(t,T.MULTIPOINT);if(null!==r)return this._renderLayersMultiPoint(r)}throw new Error("getPosition not GeoArrow point or multipoint")}_renderLayersPoint(t){const{data:o}=this.props;this.props._validate&&(e.assert(c.vector.isPointVector(t)),m(this.props,o));const[s,i]=L(this.props,["getPosition"]),a=v(o.data),n=[];for(let e=0;e<o.batches.length;e++){const l=t.data[e],d=c.child.getPointChild(l).values,h={...St,...i,recordBatchIdx:e,tableOffsets:a,id:`${this.props.id}-geoarrow-scatterplot-${e}`,data:{data:o.batches[e],length:l.length,attributes:{getPosition:{value:d,size:l.type.listSize}}}};for(const[t,r]of Object.entries(s))p({props:h,propName:t,propInput:r,chunkIdx:e});const g=new r.ScatterplotLayer(this.getSubLayerProps(h));n.push(g)}return n}_renderLayersMultiPoint(t){const{data:o}=this.props;this.props._validate&&(e.assert(c.vector.isMultiPointVector(t)),m(this.props,o));const[s,i]=L(this.props,["getPosition"]),a=v(o.data),n=[];for(let e=0;e<o.batches.length;e++){const l=t.data[e],d=c.child.getMultiPointChild(l),h=l.valueOffsets,g=c.child.getPointChild(d).values,u={...St,...i,recordBatchIdx:e,tableOffsets:a,id:`${this.props.id}-geoarrow-scatterplot-${e}`,data:{data:o.batches[e],invertedGeomOffsets:P(h),length:d.length,attributes:{getPosition:{value:g,size:d.type.listSize}}}};for(const[t,r]of Object.entries(s))p({props:u,propName:t,propInput:r,chunkIdx:e,geomCoordOffsets:h});const f=new r.ScatterplotLayer(this.getSubLayerProps(u));n.push(f)}return n}}const{data:Ct,getPosition:xt,getText:kt,getTextAnchor:Tt,getAlignmentBaseline:Mt,getPixelOffset:At,...Vt}=r.TextLayer.defaultProps,Nt={getTextAnchor:"middle",getAlignmentBaseline:"center",getPixelOffset:[0,0],_validate:!0},Gt={...Vt,...Nt};class zt extends e.CompositeLayer{static defaultProps=Gt;static layerName="GeoArrowTextLayer";getPickingInfo(t){return w(t,this.props.data)}renderLayers(){const{data:t}=this.props;if(void 0!==this.props.getPosition){const t=this.props.getPosition;if(void 0!==t&&c.vector.isPointVector(t))return this._renderLayersPoint(t);throw new Error("getPosition should pass in an arrow Vector of Point type")}{const e=g(t,T.POINT);if(null!==e)return this._renderLayersPoint(e)}throw new Error("getPosition not GeoArrow point")}_renderLayersPoint(t){const{data:o}=this.props;this.props._validate&&(e.assert(c.vector.isPointVector(t)),m(this.props,o));const[s,i]=L(this.props,["getPosition","getText"]),a=v(o.data),n=[];for(let e=0;e<o.batches.length;e++){const l=t.data[e],d=c.child.getPointChild(l).values,g=this.props.getText.data[e],u=g.values,f=g.valueOffsets,y={...Nt,...i,recordBatchIdx:e,tableOffsets:a,id:`${this.props.id}-geoarrow-heatmap-${e}`,data:{data:o.batches[e],length:l.length,startIndices:f,attributes:{getPosition:{value:h(d,l.type.listSize,f),size:l.type.listSize},getText:{value:u}}}};for(const[t,r]of Object.entries(s))p({props:y,propName:t,propInput:r,chunkIdx:e,geomCoordOffsets:f});const P=new r.TextLayer(this.getSubLayerProps(y));n.push(P)}return n}}const{data:Et,getPath:Wt,...jt}=Z,$t={_pathType:"open",_validate:!0},Bt={...jt,...$t};class Ut extends e.CompositeLayer{static defaultProps=Bt;static layerName="GeoArrowTripsLayer";renderLayers(){const{data:t}=this.props;if(void 0!==this.props.getPath){const t=this.props.getPath;if(void 0!==t&&c.vector.isLineStringVector(t))return this._renderLayersLineString(t);throw new Error("getPath should be an arrow Vector of LineString type")}{const e=g(t,T.LINESTRING);if(null!==e)return this._renderLayersLineString(e)}throw new Error("getPath not GeoArrow LineString")}_renderLayersLineString(t){const{data:r}=this.props,o=this.props.getTimestamps;this.props._validate&&(e.assert(c.vector.isLineStringVector(t)),m(this.props,r));const[s,a]=L(this.props,["getPath","getTimestamps"]),n=v(r.data),l=[];for(let e=0;e<r.batches.length;e++){const d=t.data[e],h=d.valueOffsets,g=c.child.getLineStringChild(d),u=g.type.listSize,f=c.child.getPointChild(g).values,y=o.data[e].children[0].values,P={...$t,...a,recordBatchIdx:e,tableOffsets:n,id:`${this.props.id}-geoarrow-trip-${e}`,data:{data:r.batches[e],length:d.length,startIndices:h,attributes:{getPath:{value:f,size:u},getTimestamps:{value:y,size:1}}}};for(const[t,r]of Object.entries(s))p({props:P,propName:t,propInput:r,chunkIdx:e,geomCoordOffsets:h});const L=new i.TripsLayer(P);l.push(L)}return l}}t.GeoArrowArcLayer=k,t.GeoArrowColumnLayer=z,t.GeoArrowHeatmapLayer=Y,t.GeoArrowPathLayer=tt,t.GeoArrowPointCloudLayer=at,t.GeoArrowPolygonLayer=vt,t.GeoArrowScatterplotLayer=It,t.GeoArrowSolidPolygonLayer=ht,t.GeoArrowTripsLayer=Ut,t._GeoArrowH3HexagonLayer=U,t._GeoArrowTextLayer=zt}));
//# sourceMappingURL=dist.umd.js.map
