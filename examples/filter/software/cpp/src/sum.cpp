
#include <arrow/api.h>
#include <fletcher/api.h>

#include <memory>
#include <iostream>
#include <chrono>
#include <fstream>


/**
 * Main function for the Filter example.
 */
int main(int argc, char **argv) {
  // Check number of arguments.
  if (argc < 2) {
    std::cerr << "Incorrect number of arguments. Usage: sum path/to/recordbatch.rb" << std::endl;
    return -1;
  }

  std::vector<std::shared_ptr<arrow::RecordBatch>> batches;
  std::shared_ptr<arrow::RecordBatch> number_batch;

  // Attempt to read the RecordBatch from the supplied argument.
  fletcher::ReadRecordBatchesFromFile(argv[1], &batches);

  std::ofstream outfile;
  
  if (argc == 3) {
  	std::cout << "Opening output CSV" << std::endl;
    outfile.open (argv[2], std::ios_base::app | std::ios_base::out);
  }
  


  // RecordBatch should contain exactly one batch.
  if (batches.size() != 1) {
    std::cerr << "File did not contain any Arrow RecordBatches." << std::endl;
    return -1;
  }

  number_batch = batches[0];

  fletcher::Status status;
  std::shared_ptr<fletcher::Platform> platform;
  std::shared_ptr<fletcher::Context> context;

  auto t_total_start = std::chrono::high_resolution_clock::now();

  // Create a Fletcher platform object, attempting to autodetect the platform.
  auto t_platform_create_start = std::chrono::high_resolution_clock::now();
  status = fletcher::Platform::Make(&platform);
  auto t_platform_create_end = std::chrono::high_resolution_clock::now();

  if (!status.ok()) {
    std::cerr << "Could not create Fletcher platform." << std::endl;
    return -1;
  }

  // Initialize the platform.
  auto t_platform_init_start = std::chrono::high_resolution_clock::now();
  status = platform->Init();
  auto t_platform_init_end = std::chrono::high_resolution_clock::now();

  if (!status.ok()) {
    std::cerr << "Could not create Fletcher platform." << std::endl;
    return -1;
  }

  // Create a context for our application on the platform.
  auto t_context_start = std::chrono::high_resolution_clock::now();
  status = fletcher::Context::Make(&context, platform);
  auto t_context_end = std::chrono::high_resolution_clock::now();

  if (!status.ok()) {
    std::cerr << "Could not create Fletcher context." << std::endl;
    return -1;
  }

  // Queue the recordbatch to our context.
  auto t_rb_start = std::chrono::high_resolution_clock::now();
  status = context->QueueRecordBatch(number_batch);
  auto t_rb_end = std::chrono::high_resolution_clock::now();

  if (!status.ok()) {
    std::cerr << "Could not queue the RecordBatch to the context." << std::endl;
    return -1;
  }

  // "Enable" the context, potentially copying the recordbatch to the device. This depends on your platform.
  // AWS EC2 F1 requires a copy, but OpenPOWER SNAP doesn't.
  auto t_enable_ctx_start = std::chrono::high_resolution_clock::now();
  context->Enable();
  auto t_enable_ctx_end = std::chrono::high_resolution_clock::now();

  if (!status.ok()) {
    std::cerr << "Could not enable the context." << std::endl;
    return -1;
  }

  // Create a kernel based on the context.
  auto t_create_krnl_start = std::chrono::high_resolution_clock::now();
  fletcher::Kernel kernel(context);
  auto t_create_krnl_end = std::chrono::high_resolution_clock::now();

  // Reset the kernel
  auto t_reset_krnl_start = std::chrono::high_resolution_clock::now();
  status = kernel.Reset();
  auto t_reset_krnl_end = std::chrono::high_resolution_clock::now();

  if (!status.ok()) {
    std::cerr << "Could not reset the kernel." << std::endl;
    return -1;
  }

  // Start the kernel.
  auto t_start_krnl_start = std::chrono::high_resolution_clock::now();
  status = kernel.Start();
  auto t_start_krnl_end = std::chrono::high_resolution_clock::now();

  if (!status.ok()) {
    std::cerr << "Could not start the kernel." << std::endl;
    return -1;
  }

  // Wait for the kernel to finish.
  auto t_poll_krnl_start = std::chrono::high_resolution_clock::now();
  status = kernel.WaitForFinish();
  auto t_poll_krnl_end = std::chrono::high_resolution_clock::now();

  //std::cin.ignore();

  if (!status.ok()) {
    std::cerr << "Something went wrong waiting for the kernel to finish." << std::endl;
    return -1;
  }

  // Obtain the return value.
  uint32_t return_value_0;
  uint32_t return_value_1;
  auto t_get_return_start = std::chrono::high_resolution_clock::now();
  status = kernel.GetReturn(&return_value_0, &return_value_1);
  auto t_get_return_end = std::chrono::high_resolution_clock::now();

  if (!status.ok()) {
    std::cerr << "Could not obtain the return value." << std::endl;
    return -1;
  }

  auto t_total_end = std::chrono::high_resolution_clock::now();


  //Calculate durations
  auto t_platform_create = std::chrono::duration_cast<std::chrono::microseconds>(t_platform_create_end - t_platform_create_start).count();
  auto t_platform_init = std::chrono::duration_cast<std::chrono::microseconds>(t_platform_init_end - t_platform_init_start).count();
  auto t_context = std::chrono::duration_cast<std::chrono::microseconds>(t_context_end - t_context_start).count();
  auto t_rb = std::chrono::duration_cast<std::chrono::microseconds>(t_rb_end - t_rb_start).count();
  auto t_enable_ctx = std::chrono::duration_cast<std::chrono::microseconds>(t_enable_ctx_end - t_enable_ctx_start).count();
  auto t_create_krnl = std::chrono::duration_cast<std::chrono::microseconds>(t_create_krnl_end - t_create_krnl_start).count();
  auto t_reset_krnl = std::chrono::duration_cast<std::chrono::microseconds>(t_reset_krnl_end - t_reset_krnl_start).count();
  auto t_start_krnl = std::chrono::duration_cast<std::chrono::microseconds>(t_start_krnl_end - t_start_krnl_start).count();
  auto t_poll_krnl = std::chrono::duration_cast<std::chrono::microseconds>(t_poll_krnl_end - t_poll_krnl_start).count();
  auto t_get_return = std::chrono::duration_cast<std::chrono::microseconds>(t_get_return_end - t_get_return_start).count();
  auto t_total = std::chrono::duration_cast<std::chrono::microseconds>(t_total_end - t_total_start).count();


  // Print the return value.
  std::cout << *reinterpret_cast<int32_t*>(&return_value_0) << std::endl;

  // Print durations.
  std::cout << "Create platform: \t" << t_platform_create << "us" << std::endl;
  std::cout << "Init platform: \t\t" << t_platform_init << "us" << std::endl;
  std::cout << "Create context: \t" << t_context << "us" << std::endl;
  std::cout << "Queue recordbatch: \t" << t_rb << "us" << std::endl;
  std::cout << "Enable context: \t" << t_enable_ctx << "us" << std::endl;
  std::cout << "Create kernel: \t\t" << t_create_krnl << "us" << std::endl;
  std::cout << "Reset kernel: \t\t" << t_reset_krnl << "us" << std::endl;
  std::cout << "Start kernel: \t\t" << t_start_krnl << "us" << std::endl;
  std::cout << "Waiting for kernel: \t" << t_poll_krnl << "us" << std::endl;
  std::cout << "Get return value: \t" << t_get_return << "us" << std::endl;
  std::cout << "Total runtime: \t\t" << t_total << "us" << std::endl;


  if (argc == 3) {
   outfile << argv[1] << ",";
   outfile << t_platform_create << ",";
   outfile << t_platform_init << ",";
   outfile << t_context << ",";
   outfile << t_rb << ",";
   outfile << t_enable_ctx << ",";
   outfile << t_create_krnl << ",";
   outfile << t_reset_krnl << ",";
   outfile << t_start_krnl << ",";
   outfile << t_poll_krnl << ",";
   outfile << t_get_return << ",";
   outfile << t_total << std::endl;
  }

 outfile.close();

  return 0;
}
