
function gen() {
    var fs = [];
    for (var i = 0; i < 10; i++) {
        fs.push(function () {
            return i;
        });
    }
    return fs;
}

var gs = gen();

console.log(gs[0]());
console.log(gs[1]());
console.log(gs[8]());
console.log(gs[9]());
