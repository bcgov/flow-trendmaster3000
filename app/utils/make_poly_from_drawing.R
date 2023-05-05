make_poly_from_drawing = function(coordinates){

  drawn_poly = as.data.frame(matrix(unlist(coordinates), ncol = 2, byrow = T))

  names(drawn_poly) = c("lon","lat")

  drawn_poly = st_as_sf(drawn_poly, coords = c("lon","lat"), crs = 4326)

  drawn_poly = drawn_poly %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON") %>%
    mutate(shape_name = 'drawn_poly')
}


