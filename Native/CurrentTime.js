Elm.Native.CurrentTime = {};
Elm.Native.CurrentTime.make = function(localRuntime) {

  localRuntime.Native = localRuntime.Native || {};
  localRuntime.Native.CurrentTime = localRuntime.Native.CurrentTime || {};
  if (localRuntime.Native.CurrentTime.values)
  {
    return localRuntime.Native.CurrentTime.values;
  }

  var Task = Elm.Native.Task.make(localRuntime);

  var getCurrentTime = Task.asyncFunction(function(callback) {
    return callback(Task.succeed(Date.now()));
  });

  return localRuntime.Native.CurrentTime.values = {
    getCurrentTime: getCurrentTime
  };
};
