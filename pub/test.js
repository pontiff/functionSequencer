mainVideo.stateMap = [
[
function() {
elements.narratorVideo.pause();
elements.narratorVideo.takeDowns[elements.narratorVideo.state]();},
function() {
elements.narratorVideo.currentTime = 0;
elements.narratorVideo.timeline[0]();}
]
[
function() {
testtakedown},
function() {
testetup}
]
]