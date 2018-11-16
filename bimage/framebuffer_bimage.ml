open Bimage

module Make (FB : Framebuffer.S) = struct
  let draw_image (fb : FB.t) image : unit =
    let scale a = int_of_float @@ (a *. 255. |> floor) |> char_of_int in
    for y = 0 to image.Image.height - 1 do
      for x = 0 to image.Image.width - 1 do
        let pixel = Image.get_pixel image x y |> Pixel.data in
        let r, g, b =
          match Data.length pixel with
          | 1 ->
            (scale pixel.{0}, scale pixel.{0}, scale pixel.{0})
          | 3 ->
            (scale pixel.{0}, scale pixel.{1}, scale pixel.{2})
          | _ ->
            assert false
        in
        let color = FB.compile_rgb ~r ~g ~b fb in
        FB.pixel fb ~x ~y (color : FB.color)
      done
    done
end
