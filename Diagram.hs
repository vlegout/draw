
module Diagram (run) where

import Graphics.Rendering.Cairo
import Graphics.Rendering.Pango.Cairo
import Graphics.Rendering.Pango.Enums
import Graphics.Rendering.Pango.Layout
import Graphics.Rendering.Pango.Markup

import Tasks

margin_x_e = 20.0

width_total = margin_x_left + unit_width * tot_length + margin_x_e

totalHeight = margin_top + height_sched * (fromIntegral (length diagrams)) + margin_bottom

drawArrowRight :: Double -> Double -> Render ()
drawArrowRight x y = do
               drawLine x y (x - 4) (y - 3)
               drawLine x y (x - 4) (y + 3)

drawArrowUp :: Double -> Double -> Render ()
drawArrowUp x y = do
            drawLine x y (x + 3) (y + 4)
            drawLine x y (x - 3) (y + 4)

printText :: Double -> Double -> String -> Size -> Render ()
printText x y s size = do
          layout <- createLayout ""
          liftIO $ layoutSetMarkup layout $ markSpan [FontSize size] s
          (PangoRectangle _ _ textWidth _, _) <- liftIO $ layoutGetExtents layout
          moveTo (x - textWidth / 2) y
          showLayout layout

drawLine :: Double -> Double -> Double -> Double -> Render ()
drawLine x1 y1 x2 y2 = do
         moveTo x1 y1
         lineTo x2 y2

printGrid :: Double -> Double -> Double -> Double -> Render ()
printGrid x y x_max x_grid
          | x >= x_max = draw
          | otherwise = do draw
                           printGrid (x + x_grid * unit_width) y x_max x_grid
          where draw = drawLine x y x (y - 2)

printXAxis :: Double -> Double -> Double -> Double -> Render ()
printXAxis x y x_max x_axis
           | print_x_scale == False = stroke
           | x >= x_max = print
           | otherwise = do print
                            printXAxis (x + x_axis * unit_width) y x_max x_axis
           where x' = (x - margin_x_left) / unit_width
                 print = printText x y (show (ceiling x')) SizeUnreadable

drawBase :: Diagram -> Double -> Render ()
drawBase (Diagram title x_axis x_grid y_axis) i = do
         let w = margin_x_left
             h = margin_top + height_sched * i
             h_base = height_sched / 10

         moveTo w (h + h_base * 1)
         lineTo w (h + h_base * 9)

         moveTo w (h + h_base * 9)
         lineTo (width_total - margin_x_e + 10) (h + h_base * 9)

         drawArrowUp w (h + h_base * 1)

         drawArrowRight (width_total - margin_x_e + 10) (h + h_base * 9)

         printGrid w (h + h_base * 9) (width_total - margin_x_e) x_grid

         printText 10 (h + height_sched / 2 - 10) y_axis SizeTiny
         printText (width_total - margin_x_e + 15) (h + h_base * 9 - 8) x_axis SizeTiny
         printText (width_total / 2) (h + h_base) title SizeTiny

         setLineWidth 1
         stroke

drawBases :: [Diagram] -> Double -> Render ()
drawBases [] i = fill
drawBases (x:xs) i = do drawBase x i
                        drawBases xs (i + 1)

drawHorizontal :: Double -> Double -> Double -> Double -> Render ()
drawHorizontal x y x_max y_max
               | y + 4 >= y_max = horizontal x y x_max
               | otherwise      = do horizontal x y x_max
                                     drawHorizontal x (y + 4) x_max y_max
               where horizontal x y y_max = do moveTo x y
                                               lineTo x_max y
                                               setLineWidth 0.2
                                               stroke

drawVertical :: Double -> Double -> Double -> Double -> Render ()
drawVertical x y x_max y_max
             | x + 4 >= x_max = vertical x y y_max
             | otherwise      = do vertical x y y_max
                                   drawVertical (x + 4) y x_max y_max
             where vertical x y y_max = do moveTo x y
                                           lineTo x y_max
                                           setLineWidth 0.2
                                           stroke

drawCross :: Double -> Double -> Render ()
drawCross x y = do moveTo (x - 1) y
                   lineTo (x + 1) y
                   moveTo x (y - 1)
                   lineTo x (y + 1)
                   stroke

drawCircle :: Double -> Double -> Render ()
drawCircle x y = do arc x y 0.6 0 (2 * pi)
                    fill

drawSymbolRec :: Double -> Double -> Double -> Int -> Render ()
drawSymbolRec x y y_max kind
             | y + 8 >= y_max = doDraw x y
             | otherwise      = do doDraw x y
                                   drawSymbolRec x (y + 8) y_max kind
             where doDraw x y | kind == 4 = drawCircle x y
                              | otherwise = drawCross x y

drawSymbol :: Double -> Double -> Double -> Double -> Int -> Render ()
drawSymbol x y x_max y_max kind
           | x + 8 >= x_max = drawSymbolRec x y y_max kind
           | otherwise      = do drawSymbolRec x y y_max kind
                                 drawSymbol (x + 8) y x_max y_max kind

drawExec :: Exec -> Render ()
drawExec (Exec start end color desc diagram kind size execHeight alpha) = do
         let xx1 = start * unit_width + margin_x_left
             xx2 = end * unit_width + margin_x_left
             h = margin_top + height_sched * (diagram - 1)
             h_base = height_sched / 10
             y = h + h_base * 2
             exec_width = xx2 - xx1
             exec_height = h_base * 7 * execHeight
             -- http://colorbrewer2.org/
             colorTable = [
                          [ [1, 1, 1] ],
                          [ [0.96862745, 0.98823529, 0.7254902]
                          , [0.19215686, 0.63921569, 0.32941176]],
                          [ [0.96862745, 0.98823529, 0.7254902]
                          , [0.67843137, 0.86666667, 0.55686275]
                          , [0.19215686, 0.63921569, 0.32941176]],
                          [ [1, 1, 0.8]
                          , [0.76078431, 0.90196078, 0.6]
                          , [0.47058824, 0.77647059, 0.47058824]
                          , [0.1372549, 0.51764706, 0.2627451]],
                          [ [1, 1, 0.8]
                          , [0.76078431, 0.90196078, 0.6]
                          , [0.47058824, 0.77647059, 0.47058824]
                          , [0.19215686, 0.63921569, 0.32941176]
                          , [0, 0.40784314, 0.21568627]],
                          [ [1, 1, 0.8]
                          , [0.85098039, 0.94117647, 0.63921569]
                          , [0.67843137, 0.86666667, 0.55686275]
                          , [0.47058824, 0.77647059, 0.47058824]
                          , [0.19215686, 0.63921569, 0.32941176]
                          , [0, 0.40784314, 0.21568627]],
                          [ [1, 1, 0.8]
                          , [0.85098039, 0.94117647, 0.63921569]
                          , [0.67843137, 0.86666667, 0.55686275]
                          , [0.47058824, 0.77647059, 0.47058824]
                          , [0.25490196, 0.67058824, 0.36470588]
                          , [0.1372549, 0.51764706, 0.2627451]
                          , [0, 0.35294118, 0.19607843]],
                          [ [1, 1, 0.89803922]
                          , [0.96862745, 0.98823529, 0.7254902]
                          , [0.85098039, 0.94117647, 0.63921569]
                          , [0.67843137, 0.86666667, 0.55686275]
                          , [0.47058824, 0.77647059, 0.4745098]
                          , [0.25490196, 0.67058824, 0.36470588]
                          , [0.1372549, 0.51764706, 0.2627451]
                          , [0, 0.35294118, 0.19607843]]
                          ]

         setSourceRGBA (((colorTable !! ((fromIntegral n_tasks) - 1)) !! color) !! 0)
                       (((colorTable !! ((fromIntegral n_tasks) - 1)) !! color) !! 1)
                       (((colorTable !! ((fromIntegral n_tasks) - 1)) !! color) !! 2)
                       alpha

         rectangle xx1 (y + h_base * 7 * (1 - execHeight)) exec_width exec_height
         fill

         setSourceRGBA 0 0 0 1

         setLineWidth 0.2

         case kind of 1 -> drawHorizontal xx1 (y + 4) xx2 (y + exec_height)
                      2 -> drawVertical (xx1 + 4) y xx2 (y + exec_height)
                      3 -> drawSymbol (xx1 + 2) (y + 2) xx2 (y + exec_height) kind
                      4 -> drawSymbol (xx1 + 2) (y + 2) xx2 (y + exec_height) kind
                      _ -> stroke

         rectangle xx1 (y + h_base * 7 * (1 - execHeight)) exec_width exec_height
         stroke

         printText (xx1 + ((xx2 - xx1) / 2)) (h - 2 + h_base * 3) desc size

drawVLine :: VLine -> Double -> Double -> Int -> Render ()
drawVLine line cur max t
          | cur >= max = stroke
          | otherwise = do moveTo x cur
                           lineTo x next
                           setLineWidth 0.5
                           stroke
                           drawVLine line (next + 2) max (mod (t + 1) 2)
             where next = cur + 2 + (fromIntegral t) * 6
                   x = (linex line) * unit_width + margin_x_left

drawArrow :: Arrow -> Render ()
drawArrow (Arrow x d) = do
	  let x' = margin_x_left + x * unit_width
	      h_base = height_sched / 10

	  moveTo x' (margin_top + height_sched * (d - 1) + h_base)
	  lineTo x' (margin_top + height_sched * d - 3)
	  setLineWidth 1
	  drawArrowUp x' (margin_top + height_sched * (d - 1) + h_base)
          stroke

draw :: Render ()
draw = do
     setSourceRGB 1 1 1
     paint

     setSourceRGB 0 0 0

     printXAxis margin_x_left (totalHeight - margin_bottom)
                (width_total - margin_x_e) 4

     sequence_ $ map drawExec execs
     sequence_ $ map drawArrow arrows
     sequence_ $ map (\t -> printText (textGetAbsc t) 1 (textStr t) (textSize t)) texts
     sequence_ $ map (\xs -> drawVLine xs 4 lineHeight 1) vlines

     drawBases diagrams 0

     where lineHeight = totalHeight - margin_bottom - (height_sched / 10)
           textGetAbsc t = margin_x_left + (textAbsc t) * unit_width

run :: IO ()
run = do
    withSVGSurface "output.svg" width_total totalHeight (\s -> renderWith s $ draw)
