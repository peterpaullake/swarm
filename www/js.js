function floatgreaterthanStr(x, nDigits) {
    if (nDigits === undefined) {
        nDigits = 4;
    };
    __PS_MV_REG = [];
    return Number(x).toFixed(nDigits);
};
/** Generate a list of letters from a to z. */
function genAlphabet() {
    __PS_MV_REG = [];
    return mapIndex(function (i) {
        __PS_MV_REG = [];
        return String.fromCharCode(97 + i);
    }, 26);
};
/** Generate a string made from random letters from the alphabet. */
function genRandStr(nChars) {
    if (nChars === undefined) {
        nChars = 8;
    };
    var alphabet = genAlphabet();
    var s = "";
    for (var i = 0; i < nChars; i += 1) {
        s += alphabet[Math.floor(Math.random() * nChars)];
    };
    var i = null;
    __PS_MV_REG = [];
    return s;
};
function mapIndex(fn, count) {
    var lst = [];
    for (var i = 0; i < count; i += 1) {
        lst.push(fn(i));
    };
    var i = null;
    __PS_MV_REG = [];
    return lst;
};
/** Print ARGS to the console. */
function msg() {
    var args = Array.prototype.slice.call(arguments, 0);
    var butlast = function (lst) {
        __PS_MV_REG = [];
        return lst.slice(0, -1);
    };
    var last = function (lst) {
        return lst[lst.length - 1];
    };
    var str = "";
    for (var arg = null, _js_arrvar2181 = butlast(args), _js_idx2180 = 0; _js_idx2180 < _js_arrvar2181.length; _js_idx2180 += 1) {
        arg = _js_arrvar2181[_js_idx2180];
        str = str + arg + " ";
    };
    str += last(args);
    __PS_MV_REG = [];
    return console.log(str);
};
function vecplus(v1_2182, v2_2183) {
    __PS_MV_REG = [];
    return mapcar(function (a, b) {
        return a + b;
    }, v1_2182, v2_2183);
};
function vec(v1_2184, v2_2185) {
    __PS_MV_REG = [];
    return mapcar(function (a, b) {
        return a - b;
    }, v1_2184, v2_2185);
};
function vecstar(v1_2186, v2_2187) {
    __PS_MV_REG = [];
    return mapcar(function (a, b) {
        return a * b;
    }, v1_2186, v2_2187);
};
function scaleVec(a, v) {
    __PS_MV_REG = [];
    return mapcar(function (b) {
        return a * b;
    }, v);
};
function sum(lst) {
    var s = 0;
    for (var x = null, _js_idx2188 = 0; _js_idx2188 < lst.length; _js_idx2188 += 1) {
        x = lst[_js_idx2188];
        s += x;
    };
    var x = null;
    return s;
};
function vecDot(v1, v2) {
    __PS_MV_REG = [];
    return sum(mapcar(function (a, b) {
        return a * b;
    }, v1, v2));
};
function vecLen2(v) {
    __PS_MV_REG = [];
    return vecDot(v, v);
};
function computePredDxdt(predXy, preyCoords, c, p) {
    if (c === undefined) {
        c = C;
    };
    if (p === undefined) {
        p = P;
    };
    var n = preyCoords.length;
    var z = predXy;
    __PS_MV_REG = [];
    return scaleVec(c / n, (s2189 = null, (n === 0 ? (function () {
        throw "Empty vector sum";
    })() : null, (function () {
        var xk;
        for (var k = 0; k < n; k += 1) {
            var delta2190 = (xk = preyCoords[k], scaleVec(1 / Math.pow(vecLen2(vec(xk, z)), p / 2), vec(xk, z)));
            if (s2189 === null) {
                s2189 = delta2190;
            } else {
                s2189 = vecplus(s2189, delta2190);
            };
        };
        var k = null;
        __PS_MV_REG = [];
        return s2189;
    })())));
};
function computePreyDxdts(predXy, preyCoords, a, b) {
    if (a === undefined) {
        a = A;
    };
    if (b === undefined) {
        b = B;
    };
    var n = preyCoords.length;
    var computePreyDxdt = function (j) {
        var z = predXy;
        var xj = preyCoords[j];
        __PS_MV_REG = [];
        return vecplus(scaleVec(1 / n, (s2191 = null, (n === 0 ? (function () {
            throw "Empty vector sum";
        })() : null, (function () {
            var xk;
            for (var k = 0; k < n; k += 1) {
                var delta2192 = k === j ? [0, 0] : (xk = preyCoords[k], vec(scaleVec(1 / vecLen2(vec(xj, xk)), vec(xj, xk)), scaleVec(a, vec(xj, xk))));
                if (s2191 === null) {
                    s2191 = delta2192;
                } else {
                    s2191 = vecplus(s2191, delta2192);
                };
            };
            var k = null;
            __PS_MV_REG = [];
            return s2191;
        })()))), scaleVec(b / vecLen2(vec(xj, z)), vec(xj, z)));
    };
    __PS_MV_REG = [];
    return mapIndex(computePreyDxdt, preyCoords.length);
};
/**
 * Convert a 3d color with components
 *        ranging from 0 to 1 to CSS rgb notation.
 */
function colorgreaterthanStr(color) {
    var comps = mapcar(function (x) {
        __PS_MV_REG = [];
        return Math.round(255 * x);
    }, color);
    __PS_MV_REG = [];
    return "rgb(" + comps[0] + "," + comps[1] + "," + comps[2] + ")";
};
function clearCanvas(ctx, color) {
    if (color === undefined) {
        color = [0.1, 0.1, 0.1];
    };
    ctx.fillStyle = colorgreaterthanStr(color);
    __PS_MV_REG = [];
    return ctx.fillRect(0, 0, ctx.canvas.width, ctx.canvas.height);
};
/** Add a user-adjustable parameter to the page. */
function extendParamsTable(description, type, callbackName, value, min, max) {
    var input = document.createElement("input");
    var p = document.createElement(type === "link" ? "a" : "p");
    var tdLeft = document.createElement("td");
    var tdRight = document.createElement("td");
    var tr = document.createElement("tr");
    var paramsTable = document.getElementById("params-table");
    if (type === "checkbox") {
        input.setAttribute("type", "checkbox");
    } else if (type === "range") {
        input.setAttribute("type", "range");
    };
    if (type === "checkbox") {
        if (value) {
            input.setAttribute("checked", "");
        };
    } else if (type === "range") {
        input.setAttribute("value", Math.round(100 * ((value - min) / (max - min))));
    };
    if (type === "range") {
        labelId = genRandStr();
    };
    if (type === "checkbox") {
        input.setAttribute("onchange", callbackName + "(this.checked)");
    } else if (type === "link") {
        p.setAttribute("href", "javascript:;");
        p.setAttribute("onclick", callbackName + "()");
    } else if (type === "range") {
        input.setAttribute("onchange", "x = " + min + " + " + "this.value / 100 * " + (max - min) + "; " + "document.getElementById(\"" + labelId + "\").innerHTML = " + "floatgreaterthanStr(x);" + "; " + callbackName + "(x);");
        input.setAttribute("step", 0.01);
    };
    p.setAttribute("class", "text");
    p.innerHTML = description;
    if (type !== "link") {
        tdLeft.appendChild(input);
    };
    if (type === "range") {
        var label = document.createElement("p");
        label.setAttribute("id", labelId);
        label.setAttribute("class", "text no-top-margin");
        label.innerHTML = floatgreaterthanStr(value);
        tdLeft.appendChild(label);
    };
    tdRight.appendChild(p);
    tr.appendChild(tdLeft);
    tr.appendChild(tdRight);
    __PS_MV_REG = [];
    return paramsTable.appendChild(tr);
};
function callbackName2193() {
    __PS_MV_REG = [];
    return initCoords(GAMESTATE);
};
extendParamsTable("Reset simulation", "link", "callbackName2193");
function callbackName2194() {
    GAMESTATE.lockViewToPredwhat = true;
    return GAMESTATE.lockViewToPreywhat = false;
};
extendParamsTable("Lock view to predator", "link", "callbackName2194");
function callbackName2195() {
    GAMESTATE.lockViewToPredwhat = false;
    return GAMESTATE.lockViewToPreywhat = true;
};
extendParamsTable("Lock view to prey", "link", "callbackName2195");
function callbackName2196() {
    GAMESTATE.lockViewToPredwhat = false;
    return GAMESTATE.lockViewToPreywhat = false;
};
extendParamsTable("Unlock view", "link", "callbackName2196");
var SIMSPEED = 0.001;
function callbackName2197(value) {
    return SIMSPEED = value;
};
extendParamsTable("Simulation speed", "range", "callbackName2197", 0.001, 0.0001, 0.0012);
var NPREY = 40;
function callbackName2198(value) {
    NPREY = value;
    __PS_MV_REG = [];
    return initCoords(GAMESTATE);
};
extendParamsTable("N (number of prey)", "range", "callbackName2198", 40, 1, 500);
var A = 0.796;
function callbackName2199(value) {
    return A = value;
};
extendParamsTable("a (swarm->swarm attraction)", "range", "callbackName2199", 0.796, 0, 10);
var B = 0.708;
function callbackName2200(value) {
    return B = value;
};
extendParamsTable("b (prey->predator repulsion)", "range", "callbackName2200", 0.708, 0, 10);
var C = 15;
function callbackName2201(value) {
    return C = value;
};
extendParamsTable("c (predator->prey attraction)", "range", "callbackName2201", 15, 0, 20);
var P = 2.1985;
function callbackName2202(value) {
    return P = value;
};
extendParamsTable("p (predator->prey attraction falloff)", "range", "callbackName2202", 2.1985, 0, 5);
var SHOWHELP = false;
function callbackName2203(value) {
    SHOWHELP = value;
    var helpTable = document.getElementById("help-table");
    __PS_MV_REG = [];
    return helpTable.style.visibility = value ? "visible" : "hidden";
};
extendParamsTable("Show help", "checkbox", "callbackName2203", false);
function getViewLeft(viewWidth, viewXy) {
    return viewXy[0] - viewWidth / 2;
};
function getViewBottom(viewHeight, viewXy) {
    return viewXy[1] - viewHeight / 2;
};
function drawGrid(state, color, width) {
    if (color === undefined) {
        color = [0.05, 0.05, 0.05];
    };
    if (width === undefined) {
        width = 0.2;
    };
    var state2204 = state;
    var canvas2205 = state2204.canvas;
    var ctx = canvas2205.getContext("2d");
    var viewWidth2206 = state2204.viewWidth;
    var viewXy2207 = state2204.viewXy;
    var canvasAspect = canvas2205.width / canvas2205.height;
    var viewHeight = viewWidth2206 / canvasAspect;
    var viewLeft = getViewLeft(viewWidth2206, viewXy2207);
    var viewBottom = getViewBottom(viewHeight, viewXy2207);
    var drawSeg = function (x, verticalwhat) {
        if (verticalwhat === undefined) {
            verticalwhat = true;
        };
        ctx.strokeStyle = colorgreaterthanStr(color);
        ctx.beginPath();
        if (verticalwhat) {
            ctx.moveTo(x, 0);
            ctx.lineTo(x, canvas2205.height);
        } else {
            ctx.moveTo(0, x);
            ctx.lineTo(canvas2205.width, x);
        };
        __PS_MV_REG = [];
        return ctx.stroke();
    };
    var x0 = Math.ceil(viewLeft / width) * width;
    var y0 = Math.ceil(viewBottom / width) * width;
    var x = x0;
    var y = y0;
    for (; !(x > viewLeft + viewWidth2206 && y > viewBottom + viewHeight); ) {
        var form2208 = xygreaterthanCanvasXy(state2204, [x, y]);
        var canvasX = form2208[0];
        var canvasY = form2208[1];
        drawSeg(canvasX);
        drawSeg(canvasY, null);
        var _js2209 = x + width;
        var _js2210 = y + width;
        x = _js2209;
        y = _js2210;
    };
};
function canvasXygreaterthanXy(state, xy) {
    var state2208 = state;
    var canvas2209 = state2208.canvas;
    var ctx = canvas2209.getContext("2d");
    var viewWidth2210 = state2208.viewWidth;
    var viewXy2211 = state2208.viewXy;
    var canvasAspect = canvas2209.width / canvas2209.height;
    var viewHeight = viewWidth2210 / canvasAspect;
    var viewLeft = getViewLeft(viewWidth2210, viewXy2211);
    var viewBottom = getViewBottom(viewHeight, viewXy2211);
    __PS_MV_REG = [];
    return [viewLeft + (xy[0] / canvas2209.width) * viewWidth2210, viewBottom + (xy[1] / canvas2209.height) * viewHeight];
};
function xygreaterthanCanvasXy(state, xy) {
    var state2212 = state;
    var canvas2213 = state2212.canvas;
    var ctx = canvas2213.getContext("2d");
    var viewWidth2214 = state2212.viewWidth;
    var viewXy2215 = state2212.viewXy;
    var canvasAspect = canvas2213.width / canvas2213.height;
    var viewHeight = viewWidth2214 / canvasAspect;
    var viewLeft = getViewLeft(viewWidth2214, viewXy2215);
    var viewBottom = getViewBottom(viewHeight, viewXy2215);
    __PS_MV_REG = [];
    return [((xy[0] - viewLeft) / viewWidth2214) * canvas2213.width, ((xy[1] - viewBottom) / viewHeight) * canvas2213.height];
};
function drawCircle(state, xy, color, radius) {
    if (color === undefined) {
        color = [0.5, 0.5, 0.5];
    };
    if (radius === undefined) {
        radius = 3;
    };
    var state2216 = state;
    var canvas2217 = state2216.canvas;
    var ctx = canvas2217.getContext("2d");
    var viewWidth2218 = state2216.viewWidth;
    var viewXy2219 = state2216.viewXy;
    var canvasAspect = canvas2217.width / canvas2217.height;
    var viewHeight = viewWidth2218 / canvasAspect;
    var viewLeft = getViewLeft(viewWidth2218, viewXy2219);
    var viewBottom = getViewBottom(viewHeight, viewXy2219);
    var form2220 = xygreaterthanCanvasXy(state2216, xy);
    var x = form2220[0];
    var y = form2220[1];
    ctx.fillStyle = colorgreaterthanStr(color);
    ctx.beginPath();
    ctx.arc(x, y, radius, 0, 2 * Math.PI);
    __PS_MV_REG = [];
    return ctx.fill();
};
function makePreyCoords(n) {
    var preyCoords = [];
    for (var i = 0; i < n; i += 1) {
        preyCoords.push([Math.random(), Math.random()]);
    };
    var i = null;
    __PS_MV_REG = [];
    return preyCoords;
};
function initCoords(state, nPrey) {
    if (nPrey === undefined) {
        nPrey = Math.round(NPREY);
    };
    state.predXy = [Math.random(), Math.random()];
    __PS_MV_REG = [];
    return state.preyCoords = makePreyCoords(nPrey);
};
function initGame(state, canvas, width, height) {
    if (width === undefined) {
        width = 800;
    };
    if (height === undefined) {
        height = 400;
    };
    state.canvas = canvas;
    canvas.width = width;
    canvas.height = height;
    state.viewWidth = 8;
    state.viewXy = [0.5, 0.5];
    state.lockViewToPredwhat = false;
    state.lockViewToPreywhat = false;
    initCoords(state);
    canvas.addEventListener("mousedown", function (e) {
        return state.lastClickPos = [e.clientX, e.clientY];
    });
    canvas.addEventListener("mouseleave", function (e) {
        return delete state.lastClickPos;
    });
    canvas.addEventListener("mouseup", function (e) {
        return delete state.lastClickPos;
    });
    document.addEventListener("keydown", function (e) {
        if (state.mouseXy !== undefined) {
            if (e.code == "ControlLeft") {
                return !state.lockPreywhat ? (state.lockPredwhat = true) : null;
            } else if (e.code == "ShiftLeft") {
                return !state.lockPredwhat ? (state.lockPreywhat = true) : null;
            };
        };
    });
    document.addEventListener("keyup", function (e) {
        if (state.mouseXy !== undefined) {
            if (e.code == "ControlLeft") {
                return state.lockPredwhat = false;
            } else if (e.code == "ShiftLeft") {
                return state.lockPreywhat = false;
            };
        };
    });
    canvas.addEventListener("mousemove", function (e) {
        var rect = canvas.getBoundingClientRect();
        var x = e.clientX - rect.left;
        var y = e.clientY - rect.top;
        state.mouseXy = [x, y];
        if (state.lastClickPos !== undefined) {
            var form2222 = state.lastClickPos;
            var x2223 = form2222[0];
            var y2224 = form2222[1];
            var deltaViewXy = [x2223 - e.clientX, y2224 - e.clientY];
            var state2225 = state;
            var canvas2226 = state2225.canvas;
            var ctx = canvas2226.getContext("2d");
            var viewWidth2227 = state2225.viewWidth;
            var viewXy2228 = state2225.viewXy;
            var canvasAspect = canvas2226.width / canvas2226.height;
            var viewHeight = viewWidth2227 / canvasAspect;
            var viewLeft = getViewLeft(viewWidth2227, viewXy2228);
            var viewBottom = getViewBottom(viewHeight, viewXy2228);
            state2225.viewXy = vecplus(state2225.viewXy, vecstar([viewWidth2227 / canvas2226.width, viewHeight / canvas2226.height], deltaViewXy));
            __PS_MV_REG = [];
            return state.lastClickPos = [e.clientX, e.clientY];
        };
    });
    __PS_MV_REG = [];
    return canvas.addEventListener("wheel", function (e) {
        e.preventDefault();
        __PS_MV_REG = [];
        return state.viewWidth < 0.01 ? (state.viewWidth = 0.01) : (state.viewWidth += 0.01 * e.deltaY);
    });
};
function getTime() {
    __PS_MV_REG = [];
    return performance.now();
};
function stepGame(state, dt) {
    var speed = SIMSPEED;
    var predXy2229 = state.predXy;
    var preyCoords2230 = state.preyCoords;
    var preyDxdts = computePreyDxdts(predXy2229, preyCoords2230);
    if (state.lockPredwhat) {
        state.predXy = canvasXygreaterthanXy(state, state.mouseXy);
    } else {
        state.predXy = vecplus(state.predXy, scaleVec(speed * dt, computePredDxdt(predXy2229, preyCoords2230)));
    };
    for (var i = 0; i < preyCoords2230.length; i += 1) {
        if (i === 0 && state.lockPreywhat) {
            preyCoords2230[i] = canvasXygreaterthanXy(state, state.mouseXy);
        } else {
            preyCoords2230[i] = vecplus(preyCoords2230[i], scaleVec(speed * dt, preyDxdts[i]));
        };
    };
    if (state.lockViewToPredwhat) {
        __PS_MV_REG = [];
        return state.viewXy = (state.viewXy = state.predXy);
    } else if (state.lockViewToPreywhat && state.preyCoords.length >= 1) {
        __PS_MV_REG = [];
        return state.viewXy = (state.viewXy = state.preyCoords[0]);
    };
};
function drawGame(state) {
    var state2231 = state;
    var canvas2232 = state2231.canvas;
    var ctx = canvas2232.getContext("2d");
    var viewWidth2233 = state2231.viewWidth;
    var viewXy2234 = state2231.viewXy;
    var canvasAspect = canvas2232.width / canvas2232.height;
    var viewHeight = viewWidth2233 / canvasAspect;
    var viewLeft = getViewLeft(viewWidth2233, viewXy2234);
    var viewBottom = getViewBottom(viewHeight, viewXy2234);
    clearCanvas(ctx);
    drawGrid(state2231);
    drawCircle(state2231, state2231.predXy, [1, 0, 0]);
    for (var i = 0; i < state2231.preyCoords.length; i += 1) {
        if (i === 0 && state2231.lockViewToPreywhat) {
            drawCircle(state2231, state2231.preyCoords[i], [0, 0, 1]);
        } else {
            drawCircle(state2231, state2231.preyCoords[i]);
        };
    };
};
function runGame(state) {
    var doFrame = function () {
        var dt = state.prevTime === undefined ? 0 : getTime() - state.prevTime;
        state.prevTime = getTime();
        stepGame(state, dt);
        drawGame(state);
        __PS_MV_REG = [];
        return window.requestAnimationFrame(doFrame);
    };
    __PS_MV_REG = [];
    return window.requestAnimationFrame(doFrame);
};
var GAMESTATE = {  };
initGame(GAMESTATE, document.getElementById("game-canvas"));
runGame(GAMESTATE);