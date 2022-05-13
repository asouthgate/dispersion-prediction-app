#include <Rcpp.h>
#include <cmath>
#include <iostream>
using namespace Rcpp;

//
//
// NumericMatrix& irr array to fill
// int ri_lamp, int cj_lamp, positions of lamp
// float z, height of lamp
// terrain, hard_surf, and soft_surf are floor height, hard shadow casters, and transluscent casters
// absorbance: absorbance of soft casters
// pixw: width of a pixel in metres, default is 1 pixel = 1 metre^2 
// cutoff: number of pixels to consider around a light
void cal_irradiance_raycast(NumericMatrix& irr, int ri_lamp, int cj_lamp, float z, 
                            const NumericMatrix& terrain, const NumericMatrix& hard_surf, const NumericMatrix& soft_surf,
                            const float& absorbance=0.5, const float& pixw=1, const int& cutoff=200) {

    // TODO: remove this 
    float sensor_ht = 0;
    
    int m = irr.nrow();
    int n = irr.ncol();

    int minj = std::max(cj_lamp - cutoff, 0);
    int maxj = std::min(n, cj_lamp + cutoff);
    int mini = std::max(ri_lamp - cutoff, 0);
    int maxi = std::min(m, ri_lamp + cutoff);

    std::cerr << minj << " " << maxj << " " << mini << " " << maxi << std::endl;

    for (int cj = minj; cj < maxj; ++cj) {
        for (int ri = mini; ri < maxi; ++ri) {

            float xdist = (cj_lamp - cj) * pixw;
            float ydist = (ri_lamp - ri) * pixw;
            float xydist = sqrt(std::pow(xdist,2) + std::pow(ydist,2));
            float zdist = (terrain(ri_lamp, cj_lamp) + z) - (terrain(ri, cj) + sensor_ht);
            float xyzdist = sqrt(std::pow(xydist,2) + std::pow(zdist,2));
            float dist = floor(xydist + 0.5);

            // std::cerr << cj << " " << ri << " " << xyzdist << std::endl;
            // std::cerr << "condition: zdist, hard, dist " << zdist << " " << hard_surf(ri, cj) << " " << dist << std::endl;
            
            if (zdist > 0 && !(hard_surf(ri,cj) == -1) && dist > 0) {
                // std::cerr << "casting a ray" << std::endl;
                float shadow = 1;
                float shading = 0;

                for (int d = 1; d <= dist; ++d) {
                    int dii = std::round(ri + (ydist * d / dist));
                    int djj = std::round(cj + (xdist * d / dist));
                    float hiijj = terrain(ri, cj) + sensor_ht + (d/dist) * zdist;
                    // std::cerr << "\tdcheck " << d << " " << hard_surf(dii, djj) << " " << hiijj << std::endl;
                    if (hard_surf(dii, djj) >= hiijj) {
                        shadow = 0;
                        break;
                    }
                    if (soft_surf(dii, djj) >= hiijj) {
                        shading = shading + xyzdist/xydist;
                    }
                    float invd = 1/std::pow(xyzdist,2);
                    irr(ri,cj) = (1/(std::pow(10, absorbance*shading))) * shadow * invd;
                    // std::cerr << "irr(ri, cj)" << " " << irr(ri, cj) << std::endl;
                }
            }
        }
    }

}

// [[Rcpp::export]] 
NumericMatrix cal_irradiance(NumericMatrix lights, 
                                    NumericMatrix soft_surf, NumericMatrix hard_surf, NumericMatrix terrain,
                                    int xmin, int xmax, int ymin, int ymax) {
    
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

        int ri_lamp = std::round( ((y-ymin) / yrange) * m );
        int cj_lamp = std::round( ((x-xmin) / xrange) * n );

        // std::cout << "ri_lamp " << ri_lamp << std::endl;

        cal_irradiance_raycast(irradiance, ri_lamp, cj_lamp, z, terrain, hard_surf, soft_surf);

    }

    return irradiance;
}
