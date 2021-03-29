(function(){function r(e,n,t){function o(i,f){if(!n[i]){if(!e[i]){var c="function"==typeof require&&require;if(!f&&c)return c(i,!0);if(u)return u(i,!0);var a=new Error("Cannot find module '"+i+"'");throw a.code="MODULE_NOT_FOUND",a}var p=n[i]={exports:{}};e[i][0].call(p.exports,function(r){var n=e[i][1][r];return o(n||r)},p,p.exports,r,e,n,t)}return n[i].exports}for(var u="function"==typeof require&&require,i=0;i<t.length;i++)o(t[i]);return o}return r})()({1:[function(require,module,exports){
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
    for (var arg = null, _js_arrvar529 = butlast(args), _js_idx528 = 0; _js_idx528 < _js_arrvar529.length; _js_idx528 += 1) {
        arg = _js_arrvar529[_js_idx528];
        str = str + arg + " ";
    };
    str += last(args);
    __PS_MV_REG = [];
    return console.log(str);
};
function vecplus(v1_530, v2_531) {
    __PS_MV_REG = [];
    return mapcar(function (a, b) {
        return a + b;
    }, v1_530, v2_531);
};
function vec(v1_532, v2_533) {
    __PS_MV_REG = [];
    return mapcar(function (a, b) {
        return a - b;
    }, v1_532, v2_533);
};
function vecstar(v1_534, v2_535) {
    __PS_MV_REG = [];
    return mapcar(function (a, b) {
        return a * b;
    }, v1_534, v2_535);
};
function scaleVec(a, v) {
    __PS_MV_REG = [];
    return mapcar(function (b) {
        return a * b;
    }, v);
};
function sum(lst) {
    var s = 0;
    for (var x = null, _js_idx536 = 0; _js_idx536 < lst.length; _js_idx536 += 1) {
        x = lst[_js_idx536];
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
    return scaleVec(c / n, (s537 = null, (n === 0 ? (function () {
        throw "Empty vector sum";
    })() : null, (function () {
        var xk;
        for (var k = 0; k < n; k += 1) {
            var delta538 = (xk = preyCoords[k], scaleVec(1 / Math.pow(vecLen2(vec(xk, z)), p / 2), vec(xk, z)));
            if (s537 === null) {
                s537 = delta538;
            } else {
                s537 = vecplus(s537, delta538);
            };
        };
        var k = null;
        __PS_MV_REG = [];
        return s537;
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
        return vecplus(scaleVec(1 / n, (s539 = null, (n === 0 ? (function () {
            throw "Empty vector sum";
        })() : null, (function () {
            var xk;
            for (var k = 0; k < n; k += 1) {
                var delta540 = k === j ? [0, 0] : (xk = preyCoords[k], vec(scaleVec(1 / vecLen2(vec(xj, xk)), vec(xj, xk)), scaleVec(a, vec(xj, xk))));
                if (s539 === null) {
                    s539 = delta540;
                } else {
                    s539 = vecplus(s539, delta540);
                };
            };
            var k = null;
            __PS_MV_REG = [];
            return s539;
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
function callbackName541() {
    __PS_MV_REG = [];
    return initCoords(GAMESTATE);
};
extendParamsTable("Reset simulation", "link", "callbackName541");
function callbackName542() {
    GAMESTATE.lockViewToPredwhat = true;
    return GAMESTATE.lockViewToPreywhat = false;
};
extendParamsTable("Lock view to predator", "link", "callbackName542");
function callbackName543() {
    GAMESTATE.lockViewToPredwhat = false;
    return GAMESTATE.lockViewToPreywhat = true;
};
extendParamsTable("Lock view to prey", "link", "callbackName543");
function callbackName544() {
    GAMESTATE.lockViewToPredwhat = false;
    return GAMESTATE.lockViewToPreywhat = false;
};
extendParamsTable("Unlock view", "link", "callbackName544");
var SIMSPEED = 0.001;
function callbackName545(value) {
    return SIMSPEED = value;
};
extendParamsTable("Simulation speed", "range", "callbackName545", 0.001, 0.0001, 0.0012);
var NPREY = 40;
function callbackName546(value) {
    NPREY = value;
    __PS_MV_REG = [];
    return initCoords(GAMESTATE);
};
extendParamsTable("N (number of prey)", "range", "callbackName546", 40, 1, 500);
var A = 0.796;
function callbackName547(value) {
    return A = value;
};
extendParamsTable("a (swarm->swarm attraction)", "range", "callbackName547", 0.796, 0, 10);
var B = 0.708;
function callbackName548(value) {
    return B = value;
};
extendParamsTable("b (prey->predator repulsion)", "range", "callbackName548", 0.708, 0, 10);
var C = 15;
function callbackName549(value) {
    return C = value;
};
extendParamsTable("c (predator->prey attraction)", "range", "callbackName549", 15, 0, 20);
var P = 2.1985;
function callbackName550(value) {
    return P = value;
};
extendParamsTable("p (predator->prey attraction falloff)", "range", "callbackName550", 2.1985, 0, 5);
var SHOWHELP = false;
function callbackName551(value) {
    SHOWHELP = value;
    var helpTable = document.getElementById("help-table");
    __PS_MV_REG = [];
    return helpTable.style.visibility = value ? "visible" : "hidden";
};
extendParamsTable("Show help", "checkbox", "callbackName551", false);
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
    var state552 = state;
    var canvas553 = state552.canvas;
    var ctx = canvas553.getContext("2d");
    var viewWidth554 = state552.viewWidth;
    var viewXy555 = state552.viewXy;
    var canvasAspect = canvas553.width / canvas553.height;
    var viewHeight = viewWidth554 / canvasAspect;
    var viewLeft = getViewLeft(viewWidth554, viewXy555);
    var viewBottom = getViewBottom(viewHeight, viewXy555);
    var drawSeg = function (x, verticalwhat) {
        if (verticalwhat === undefined) {
            verticalwhat = true;
        };
        ctx.strokeStyle = colorgreaterthanStr(color);
        ctx.beginPath();
        if (verticalwhat) {
            ctx.moveTo(x, 0);
            ctx.lineTo(x, canvas553.height);
        } else {
            ctx.moveTo(0, x);
            ctx.lineTo(canvas553.width, x);
        };
        __PS_MV_REG = [];
        return ctx.stroke();
    };
    var x0 = Math.ceil(viewLeft / width) * width;
    var y0 = Math.ceil(viewBottom / width) * width;
    var x = x0;
    var y = y0;
    for (; !(x > viewLeft + viewWidth554 && y > viewBottom + viewHeight); ) {
        var form556 = xygreaterthanCanvasXy(state552, [x, y]);
        var canvasX = form556[0];
        var canvasY = form556[1];
        drawSeg(canvasX);
        drawSeg(canvasY, null);
        var _js557 = x + width;
        var _js558 = y + width;
        x = _js557;
        y = _js558;
    };
};
function canvasXygreaterthanXy(state, xy) {
    var state556 = state;
    var canvas557 = state556.canvas;
    var ctx = canvas557.getContext("2d");
    var viewWidth558 = state556.viewWidth;
    var viewXy559 = state556.viewXy;
    var canvasAspect = canvas557.width / canvas557.height;
    var viewHeight = viewWidth558 / canvasAspect;
    var viewLeft = getViewLeft(viewWidth558, viewXy559);
    var viewBottom = getViewBottom(viewHeight, viewXy559);
    __PS_MV_REG = [];
    return [viewLeft + (xy[0] / canvas557.width) * viewWidth558, viewBottom + (xy[1] / canvas557.height) * viewHeight];
};
function xygreaterthanCanvasXy(state, xy) {
    var state560 = state;
    var canvas561 = state560.canvas;
    var ctx = canvas561.getContext("2d");
    var viewWidth562 = state560.viewWidth;
    var viewXy563 = state560.viewXy;
    var canvasAspect = canvas561.width / canvas561.height;
    var viewHeight = viewWidth562 / canvasAspect;
    var viewLeft = getViewLeft(viewWidth562, viewXy563);
    var viewBottom = getViewBottom(viewHeight, viewXy563);
    __PS_MV_REG = [];
    return [((xy[0] - viewLeft) / viewWidth562) * canvas561.width, ((xy[1] - viewBottom) / viewHeight) * canvas561.height];
};
function drawCircle(state, xy, color, radius) {
    if (color === undefined) {
        color = [0.5, 0.5, 0.5];
    };
    if (radius === undefined) {
        radius = 3;
    };
    var state564 = state;
    var canvas565 = state564.canvas;
    var ctx = canvas565.getContext("2d");
    var viewWidth566 = state564.viewWidth;
    var viewXy567 = state564.viewXy;
    var canvasAspect = canvas565.width / canvas565.height;
    var viewHeight = viewWidth566 / canvasAspect;
    var viewLeft = getViewLeft(viewWidth566, viewXy567);
    var viewBottom = getViewBottom(viewHeight, viewXy567);
    var form568 = xygreaterthanCanvasXy(state564, xy);
    var x = form568[0];
    var y = form568[1];
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
            var form570 = state.lastClickPos;
            var x571 = form570[0];
            var y572 = form570[1];
            var deltaViewXy = [x571 - e.clientX, y572 - e.clientY];
            var state573 = state;
            var canvas574 = state573.canvas;
            var ctx = canvas574.getContext("2d");
            var viewWidth575 = state573.viewWidth;
            var viewXy576 = state573.viewXy;
            var canvasAspect = canvas574.width / canvas574.height;
            var viewHeight = viewWidth575 / canvasAspect;
            var viewLeft = getViewLeft(viewWidth575, viewXy576);
            var viewBottom = getViewBottom(viewHeight, viewXy576);
            state573.viewXy = vecplus(state573.viewXy, vecstar([viewWidth575 / canvas574.width, viewHeight / canvas574.height], deltaViewXy));
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
    var predXy577 = state.predXy;
    var preyCoords578 = state.preyCoords;
    var preyDxdts = computePreyDxdts(predXy577, preyCoords578);
    if (state.lockPredwhat) {
        state.predXy = canvasXygreaterthanXy(state, state.mouseXy);
    } else {
        state.predXy = vecplus(state.predXy, scaleVec(speed * dt, computePredDxdt(predXy577, preyCoords578)));
    };
    for (var i = 0; i < preyCoords578.length; i += 1) {
        if (i === 0 && state.lockPreywhat) {
            preyCoords578[i] = canvasXygreaterthanXy(state, state.mouseXy);
        } else {
            preyCoords578[i] = vecplus(preyCoords578[i], scaleVec(speed * dt, preyDxdts[i]));
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
    var state579 = state;
    var canvas580 = state579.canvas;
    var ctx = canvas580.getContext("2d");
    var viewWidth581 = state579.viewWidth;
    var viewXy582 = state579.viewXy;
    var canvasAspect = canvas580.width / canvas580.height;
    var viewHeight = viewWidth581 / canvasAspect;
    var viewLeft = getViewLeft(viewWidth581, viewXy582);
    var viewBottom = getViewBottom(viewHeight, viewXy582);
    clearCanvas(ctx);
    drawGrid(state579);
    drawCircle(state579, state579.predXy, [1, 0, 0]);
    for (var i = 0; i < state579.preyCoords.length; i += 1) {
        if (i === 0 && state579.lockViewToPreywhat) {
            drawCircle(state579, state579.preyCoords[i], [0, 0, 1]);
        } else {
            drawCircle(state579, state579.preyCoords[i]);
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
},{}]},{},[1]);
