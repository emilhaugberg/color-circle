exports.colorScale = function (perc) {
  var r = 0
  var g = 0
  var b = 0

  if (perc < 50) {
    r = 255
    g = Math.round(5.1 * perc)
  } else {
    g = 255
    r = Math.round(510 - 5.10 * perc)
  }

  var h = r * 0x10000 + g * 0x100 + b * 0x1
  return '#' + ('000000' + h.toString(16)).slice(-6)
}
