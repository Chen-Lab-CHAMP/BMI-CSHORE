from pymt_cshore import cshoremodel
import aeolis.model
import numpy as np
import traceback
import sys
import csv

def CollectIndividualVarInfo_Aeolis(model_bmi, var_name):

    return {
        var_name: {
            "var_type": model_bmi.get_var_type(var_name),
            "var_rank": model_bmi.get_var_rank(var_name),
            "var_shape": model_bmi.get_var_shape(var_name)}}


def CollectIndividualVarInfo_BmiCshore(model_bmi, var_name):

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
        info = CollectIndividualVarInfo_BmiCshore(model_bmi, var_name)
        for k in info:
            info[k]["io_type"] = "in"
        var_info.update(info)

    for var_name in var_outs:
        info = CollectIndividualVarInfo_BmiCshore(model_bmi, var_name)
        for k in info:
            info[k]["io_type"] = "out"
        var_info.update(info)

    for var_name in var_in_outs:
        info = CollectIndividualVarInfo_BmiCshore(model_bmi, var_name)
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
        # initialize CSHORE
        cshore_model = cshoremodel()
        cshore_model.initialize('./')

        var_info = CollectVarInfoViaBmi(cshore_model)
        for name, info in var_info.items():
            info["buffer"] = create_var_buffer(var_info, name)
            if info["io_type"] == "out":
                cshore_model.get_value(name, info["buffer"])

        # initialize AeoLiS
        aeolis_model=aeolis.model.AeoLiS(configfile="./aeolis.txt")
        aeolis_model.initialize()

        bathymetry_aeolis = aeolis_model.get_var("zb")

        # preserve initial bathymetry from CSHORE
        cshore_model.get_value("bathymetry", var_info["bathymetry"]["buffer"])
        with open('zb_begin.csv', 'w', newline='') as file:
            column_names = []
            for t in range(0, var_info["total_transects"]["buffer"][0], 1):
                column_names.append(f"zb_{t + 1}")
            mywriter = csv.DictWriter(file, fieldnames = column_names)
            mywriter.writeheader()

            offset = 0
            for t in range(0, var_info["total_transects"]["buffer"][0], 1):
                for g in range(0, var_info["grid_per_transect"]["buffer"][t], 1):
                    mywriter.writerow(
                        {f"zb_{t + 1}":
                            var_info["bathymetry"]["buffer"][offset + g]})
                offset += var_info["grid_per_transect"]["buffer"][t]

        # time loop
        for itime in range(1, var_info["ntime"]["buffer"][0] + 1, 1):
            print(f"Step {itime}")

            var_info["mainloop_itime"]["buffer"][0] = itime
            cshore_model.set_value('mainloop_itime', var_info["mainloop_itime"]["buffer"])
            cshore_model.update()

            # get_value from cshore
            cshore_model.get_value("bathymetry", var_info["bathymetry"]["buffer"])
            # set_value to aeolis
            offset = 0
            for t in range(0, var_info["total_transects"]["buffer"][0], 1):
                for g in range(0, var_info["grid_per_transect"]["buffer"][t], 1):
                    bathymetry_aeolis[t][g] = var_info["bathymetry"]["buffer"][offset + g]
                offset += var_info["grid_per_transect"]["buffer"][t]

            # aeolis update
            aeolis_model.update()

            # get_value from aeolis
            # set_value to cshore
            offset = 0
            for t in range(0, var_info["total_transects"]["buffer"][0], 1):
                for g in range(0, var_info["grid_per_transect"]["buffer"][t], 1):
                    var_info["bathymetry"]["buffer"][offset + g] = bathymetry_aeolis[t][g]
                offset += var_info["grid_per_transect"]["buffer"][t]

        # preserve final bathymetry from AeoLiS
        with open('zb_end.csv', 'w', newline='') as file:
            column_names = []
            for t in range(0, len(bathymetry_aeolis), 1):
                column_names.append(f"zb_{t + 1}")
            mywriter = csv.DictWriter(file, fieldnames = column_names)
            mywriter.writeheader()

            for t in range(0, len(bathymetry_aeolis), 1):
                for g in range(0, len(bathymetry_aeolis[t]), 1):
                    mywriter.writerow({f"zb_{t + 1}": bathymetry_aeolis[t][g]})

        aeolis_model.finalize()

        cshore_model.finalize()

        sys.exit(0)

    except Exception as e:
        print(str(e))
        traceback.print_exc()

        sys.exit(1)
