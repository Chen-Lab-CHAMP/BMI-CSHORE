from pymt_cshore import cshoremodel
import numpy as np
import traceback
import sys
import csv

def CollectIndividualVarInfo(model_bmi, var_name):

    data_type = model_bmi.get_var_type(var_name)
    grid_id = model_bmi.get_var_grid(var_name)
    grid_rank = model_bmi.get_grid_rank(grid_id)
    grid_shape = np.ndarray((grid_rank,), dtype = np.int32)
    model_bmi.get_grid_shape(grid_id, grid_shape)
    grid_size = model_bmi.get_grid_size(grid_id)

    return {
        var_name: {
            "data_type": data_type,
            "grid_id": grid_id,
            "grid_rank": grid_rank,
            "grid_shape": grid_shape,
            "grid_size": grid_size}}

def CollectVarInfoViaBmi(model_bmi):

    var_info = {}

    var_ins = set(model_bmi.get_input_var_names())
    var_outs = set(model_bmi.get_output_var_names())
    var_in_outs = var_ins.intersection(var_outs)
    var_ins = var_ins.difference(var_in_outs)
    var_outs = var_outs.difference(var_in_outs)

    for var_name in var_ins:
        info = CollectIndividualVarInfo(model_bmi, var_name)
        for k in info:
            info[k]["io_type"] = "in"
        var_info.update(info)

    for var_name in var_outs:
        info = CollectIndividualVarInfo(model_bmi, var_name)
        for k in info:
            info[k]["io_type"] = "out"
        var_info.update(info)

    for var_name in var_in_outs:
        info = CollectIndividualVarInfo(model_bmi, var_name)
        for k in info:
            info[k]["io_type"] = "in_out"
        var_info.update(info)

    return var_info


def create_var_buffer(var_info, var_name_string):
    var_buffer = np.ndarray(var_info[var_name_string]['grid_size'], \
        dtype=getattr(np, var_info[var_name_string]['data_type']) )

    return var_buffer

########################################################################
if __name__ == "__main__":
    try:
        cshore = cshoremodel()

        cshore.initialize('./')

        var_info = CollectVarInfoViaBmi(cshore)

        ntime_buffer = create_var_buffer(var_info, 'ntime')
        cshore.get_value('ntime', ntime_buffer)
        print('ntime=',ntime_buffer)

        total_transects_buffer = create_var_buffer(var_info, 'total_transects')
        cshore.get_value('total_transects', total_transects_buffer)
        print('total_transects=',total_transects_buffer)

        grid_per_transect_buffer = create_var_buffer(var_info, 'grid_per_transect')
        cshore.get_value('grid_per_transect', grid_per_transect_buffer)
        print('grid_per_transect=',grid_per_transect_buffer)

        bathymetry_buffer = create_var_buffer(var_info, 'bathymetry')
        cshore.get_value('bathymetry', bathymetry_buffer)
        print('bathymetry=',bathymetry_buffer[:5])
        print('bathymetry=',bathymetry_buffer[2690:2702])

        mainloop_itime_buffer= create_var_buffer(var_info, 'mainloop_itime')
        print('mainloop_itime=', mainloop_itime_buffer)

        with open('zb_init.csv', 'w', newline='') as file:
            mywriter = csv.DictWriter(file, fieldnames = ['zb'])
            mywriter.writeheader()
            for i in range(0, 2702, 1):
                mywriter.writerow({'zb': bathymetry_buffer[i]})

        # time loop
        for itime in range(1, ntime_buffer[0]+1):
            mainloop_itime_buffer[0]=itime
            cshore.set_value('mainloop_itime', mainloop_itime_buffer)
            print(mainloop_itime_buffer)
            cshore.update()
        #
        cshore.get_value('bathymetry', bathymetry_buffer)
        with open('zb_end.csv', 'w', newline='') as file:
            mywriter = csv.DictWriter(file, fieldnames = ['zb'])
            mywriter.writeheader()
            for i in range(0, 2702, 1):
                mywriter.writerow({'zb': bathymetry_buffer[i]})

        cshore.finalize()


        sys.exit(0)

    except Exception as e:
        print(str(e))
        traceback.print_exc()

        sys.exit(1)
