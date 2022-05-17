#include <Rcpp.h>
#include <cmath>
#include <iostream>
using namespace Rcpp;

// NumericMatrix& irr array to fill
// int ri_lamp, int cj_lamp, positions of lamp
// float z, height of lamp
// terrain, hard_surf, and soft_surf are floor height, hard shadow casters, and transluscent casters
// absorbance: absorbance of soft casters
// pixw: width of a pixel in metres, default is 1 pixel = 1 metre^2 
// cutoff: number of pixels to consider around a light
// sensor_ht: a height offset, should be set to zero but isnt in the old implementation
void cal_irradiance_raycast(NumericMatrix& irr, int ri_lamp, int cj_lamp, float z, 
                            const NumericMatrix& terrain, const NumericMatrix& soft_surf, const NumericMatrix& hard_surf, 
                            const float& absorbance, const float& pixw, const int& cutoff, const float& sensor_ht) {
    
    int m = irr.nrow();
    int n = irr.ncol();

    int minj = std::max(cj_lamp - cutoff, 0);
    int maxj = std::min(n, cj_lamp + cutoff);
    int mini = std::max(ri_lamp - cutoff, 0);
    int maxi = std::min(m, ri_lamp + cutoff);

    for (int cj = minj; cj < maxj; ++cj) {
        for (int ri = mini; ri < maxi; ++ri) {

            float pxdist = (cj_lamp - cj);
            float pydist = (ri_lamp - ri);
            float pxydist = sqrt(std::pow(pxdist, 2) + std::pow(pydist, 2));
            float pdist = floor(pxydist + 0.5);

            float zdist = (terrain(ri_lamp, cj_lamp) + z) - (terrain(ri, cj) + sensor_ht);
            float xydist = pxydist * pixw;
            float xyzdist = sqrt( pow(xydist, 2) + pow(zdist, 2) );
            
            // if (zdist > 0 && !(hard_surf(ri,cj) == -1) && pdist > 0) {
            if (xydist < cutoff && zdist > 0 && pdist > 0) {

                float shadow = 1.0;
                float shading = 0.0;

                for (int d = 1; d <= pdist; ++d) {
                    int dii = std::round(ri + (pydist * d / pdist));
                    int djj = std::round(cj + (pxdist * d / pdist));
                    float hiijj = terrain(ri, cj) + sensor_ht + (d/pdist) * zdist;

                    if (hard_surf(dii, djj) >= hiijj) {
                        shadow = 0;
                        break;
                    }
                    if (soft_surf(dii, djj) >= hiijj) {
                        // shading += pixw;
                        // TODO: we cant take any better than that, pxyzdist/pxydist is 1/cos(theta), not wanted
                        // at least it would b e 
                        shading = shading + pixw * xyzdist/xydist;
                    }
                }
                
                float invd = 1.0 / std::pow(xyzdist, 2);
                float occ = 1.0 / (std::pow(10, absorbance * shading));
                float v = occ * shadow * invd;
                irr(ri,cj) += v;

            }
        }
    }

}

// [[Rcpp::export]] 
NumericMatrix cal_irradiance(NumericMatrix lights, 
                                    NumericMatrix soft_surf, NumericMatrix hard_surf, NumericMatrix terrain,
                                    int xmin, int xmax, int ymin, int ymax,
                                    int abs, int pix, int cutoff, float sensor_ht) {
    
    // setup output
    int m = soft_surf.nrow();
    int n = soft_surf.ncol();
    NumericMatrix irradiance(m, n);

    int nlamps = lights.nrow();

    for (int i = 0; i < nlamps; ++i) {

        float x = lights(i, 0);
        float y = lights(i, 1);
        float z = lights(i, 2);

        float xrange = xmax - xmin;
        float yrange = ymax - ymin;

        int ri_lamp = m - std::round( ((y-ymin) / yrange) * m );
        int cj_lamp = std::round( ((x-xmin) / xrange) * n );

        cal_irradiance_raycast(irradiance, ri_lamp, cj_lamp, z, terrain, hard_surf, soft_surf, abs, pix, cutoff, sensor_ht);

    }

    return irradiance;
}
