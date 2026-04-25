#!/usr/bin/env python3
"""
Time pywfm loading of the C2VSimFG model in inquiry mode (is_for_inquiry=1)
through the new Track 7 handle-based DLL.

Two scenarios:
  - cold cache: IW_ModelData_ForInquiry.bin deleted before load, kernel
    must rebuild from raw inputs (heavy)
  - warm cache: the .bin is present, kernel deserializes it directly (fast)
"""

import os
import sys
import time

sys.path.insert(0, r"C:\Users\hatch\Desktop\pywfm\src")
import pywfm

MODEL = r"C:\Users\hatch\Desktop\c2vsimfg-track7"
PP    = os.path.join(MODEL, "Preprocessor", "C2VSimFG_Preprocessor.in")
SIM   = os.path.join(MODEL, "Simulation",   "C2VSimFG.in")
CACHE = os.path.join(MODEL, "Simulation",   "IW_ModelData_ForInquiry.bin")


def load_inquiry(label):
    os.chdir(os.path.dirname(SIM))
    t0 = time.perf_counter()
    m = pywfm.IWFMModel(
        PP, SIM,
        has_routed_streams=1,
        is_for_inquiry=1,
        delete_inquiry_data_file=False,  # we manage the cache ourselves
        log_file="inquiry_time.log",
    )
    elapsed = time.perf_counter() - t0
    print(f"[{label}] load time:  {elapsed:8.2f} s   model_id={m.model_id.value}")
    print(f"[{label}] n_nodes:        {m.get_n_nodes()}")
    print(f"[{label}] n_elements:     {m.get_n_elements()}")
    print(f"[{label}] n_time_steps:   {m.get_n_time_steps()}")
    print(f"[{label}] n_layers:       {m.get_n_layers()}")
    m.kill()
    return elapsed


def main():
    print(f"DLL:   {pywfm.LIB}")
    print(f"Model: {MODEL}")
    print()

    # ---- Warm-cache scenario (cache file present) ----
    if not os.path.exists(CACHE):
        print(f"WARN: {CACHE} missing — warm-cache scenario will rebuild it")
    else:
        size_mb = os.path.getsize(CACHE) / 1024 / 1024
        print(f"Warm cache file present ({size_mb:.1f} MB)")
    warm = load_inquiry("warm")
    print()

    # ---- Cold-cache scenario (cache deleted) ----
    if os.path.exists(CACHE):
        os.remove(CACHE)
        print(f"Deleted {CACHE}")
    cold = load_inquiry("cold")
    print()

    print("=" * 60)
    print(f"Cold load (rebuild from inputs):  {cold:8.2f} s")
    print(f"Warm load (cache deserialize):    {warm:8.2f} s")
    if warm > 0:
        print(f"Speedup from cache:               {cold/warm:6.2f}x")


if __name__ == "__main__":
    main()
