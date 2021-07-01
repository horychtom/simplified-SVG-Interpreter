# simplified-SVG-Interpreter

transforms SVG code into valid svg graphics. Example:

``` racket
(define tree-prg
  '(
    (define (draw x1 y1 x2 y2 len angle)
      (if (> len 30)
        (line x1 y1 x2 y2 "stroke:black;stroke-width:2;opacity:0.9")
        (line x1 y1 x2 y2 "stroke:green;stroke-width:3;opacity:0.9"))
      (when (> len 20)
        (recur-tree x2 y2 (floor (* len 0.7)) angle)
        (recur-tree x2 y2 (floor (* len 0.7)) (+ angle 0.3))
        (recur-tree x2 y2 (floor (* len 0.7)) (- angle 0.6)))
    )
    
  (define (recur-tree x1 y1 len angle)
    (draw x1
      y1
      (+ x1 (* len (cos angle)))
      (+ y1 (* len (sin angle)))
      len
      angle))
  )
)
```
now calling this command will evaluate int picture seen below

``` racket
(display (execute 400 400 tree-prg '(recur-tree 200 400 100 (* 3.14 1.5))))
```

![tree](https://user-images.githubusercontent.com/67648067/124168241-2929db00-daa5-11eb-9ec6-d6067048e182.png)
