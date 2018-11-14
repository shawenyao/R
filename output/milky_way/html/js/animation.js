const totalFrames = 99;
const animationDuration = 3000;
const timePerFrame = animationDuration / totalFrames;
let timeWhenLastUpdate;
let timeFromLastUpdate;
let frameNumber = 1;

// 'step' function will be called each time browser rerender the content
// we achieve that by passing 'step' as a parameter to 'requestAnimationFrame' function
function step(startTime) {
  // 'startTime' is provided by requestAnimationName function, and we can consider it as current time
  // first of all we calculate how much time has passed from the last time when frame was update
  if (!timeWhenLastUpdate) timeWhenLastUpdate = startTime;
  timeFromLastUpdate = startTime - timeWhenLastUpdate;

  // then we check if it is time to update the frame
  if (timeFromLastUpdate > timePerFrame) {
    // hide all frames
    $('.animation').css('opacity', 0);
    // and show the required one
    $(`.animation-${frameNumber}`).css('opacity', 1);
    // reset the last update time
    timeWhenLastUpdate = startTime;

    // then increase the frame number or reset it if it is the last frame
    if (frameNumber >= totalFrames) {
      frameNumber = 1;
    } else {
      frameNumber = frameNumber + 1;
    }        
  }

  requestAnimationFrame(step);
}

// wait for images to be downloaded and start the animation
$(window).on('load', () => {
  requestAnimationFrame(step);
});
