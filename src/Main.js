'use strict'

// module Main

exports.gunzip = require('zlib').createGunzip

exports.createInterfaceWithInput = function (inputStream) {
  return function () {
    return require('readline').createInterface({input: inputStream})
  }
}
