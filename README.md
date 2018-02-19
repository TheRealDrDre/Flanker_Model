# How to run the model

  1. Load the `flanker-device.lisp` file first, before loading
     any model.
  2. __After__ loading this device, load any model. A simple
     model performing visuo-motor operations is given in the
     response-monkey.lisp file.
  3. Before running the model call the `smart-reload` function:

                   CL-USER> (smart-reload)

     This will ensure that the device is properly installed
     and connected to the model.
  4. The device will produce single type of visual-location,
     called "flanker-stimulus-location", which contains a
     slot "phase" that indicates the task's phase (pause,
     stimulus, done).
  5. Every stimulus is represented by a `flanker-stimulus`
     chunk, whose kind is `stimulus` and with special slots for
     _left_, _center_, and _right_ stimuli. E.g., the stimulus "> < >"
     is represented by chunk with slots:

         left I>
         center I<
         right I>

  6. Note that the symbols ">" and "<" are represented by the
     chunks "I>" and "I<" because they cannot be used to name
     proper chunks in ACT-R.
