// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var $$Array = require("bs-platform/lib/js/array.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Printf = require("bs-platform/lib/js/printf.js");
var $$String = require("bs-platform/lib/js/string.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");

var input = $$Array.map($$String.trim, "132791\n78272\n114679\n60602\n59038\n69747\n61672\n147972\n92618\n70186\n125826\n61803\n78112\n124864\n58441\n113062\n105389\n125983\n90716\n75544\n148451\n73739\n127762\n146660\n128747\n148129\n138635\n80095\n60241\n145455\n98730\n59139\n146828\n113550\n91682\n107415\n129207\n147635\n104583\n102245\n73446\n148657\n96364\n52033\n69964\n63609\n98207\n73401\n65511\n115034\n126179\n96664\n85394\n128472\n79017\n93222\n55267\n102446\n133150\n148985\n95325\n57713\n77370\n60879\n111977\n99362\n91581\n55201\n137670\n127159\n128324\n77217\n86378\n112847\n108265\n80355\n75650\n106222\n67793\n113891\n74508\n139463\n69972\n122753\n135854\n127770\n101085\n98304\n61451\n146719\n61225\n60468\n83613\n137436\n126303\n78759\n70081\n110671\n113234\n111563".split("\n"));

function fuelReq(mass) {
  return (mass / 3 | 0) - 2 | 0;
}

function sumWith(getFuel) {
  return $$Array.fold_left((function (sum, mass) {
                return sum + Curry._1(getFuel, Caml_format.caml_int_of_string(mass)) | 0;
              }), 0, input);
}

var total1 = sumWith(fuelReq);

console.log(Curry._1(Printf.sprintf(/* Format */[
              /* String_literal */Block.__(11, [
                  "Part 1: ",
                  /* Int */Block.__(4, [
                      /* Int_d */0,
                      /* No_padding */0,
                      /* No_precision */0,
                      /* End_of_format */0
                    ])
                ]),
              "Part 1: %d"
            ]), total1));

function fuelOfFuel(mass) {
  var _mass_ = mass;
  var _sum = 0;
  while(true) {
    var sum = _sum;
    var mass_ = _mass_;
    var fuel = fuelReq(mass_);
    if (fuel <= 0) {
      return sum;
    } else {
      console.log(fuel);
      _sum = sum + fuel | 0;
      _mass_ = fuel;
      continue ;
    }
  };
}

var total2 = sumWith(fuelOfFuel);

console.log(Curry._1(Printf.sprintf(/* Format */[
              /* String_literal */Block.__(11, [
                  "Part 2: ",
                  /* Int */Block.__(4, [
                      /* Int_d */0,
                      /* No_padding */0,
                      /* No_precision */0,
                      /* End_of_format */0
                    ])
                ]),
              "Part 2: %d"
            ]), total2));

exports.input = input;
exports.fuelReq = fuelReq;
exports.sumWith = sumWith;
exports.total1 = total1;
exports.fuelOfFuel = fuelOfFuel;
exports.total2 = total2;
/* input Not a pure module */
