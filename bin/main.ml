
include Process.Output
include Unix


type time = int (* Whole seconds of Unix time *)
type program_state = MonitoringModem | MonitoringInternet | RebootingModem
type connection_state = Up of time | Down of time | Waiting of time

let now () = int_of_float (Unix.time ())


let check_exit_status process_output =
  match process_output.exit_status with
  | Process.Exit.Exit 0 -> true
  | Process.Exit.Exit _ -> false
  | Process.Exit.Kill _ -> false
  | Process.Exit.Stop _ -> false


let can_ping interface host =
  check_exit_status
    (Process.run "ping" [|"-c"; "1"; "-W"; "10"; "-I"; interface; host|])


let can_curl interface max_time host =
  check_exit_status
    (Process.run "curl" [|"--interface"; interface; "--max-time"; max_time; "--head"; host|])


let rec get_modem_status boot_duration previous_status =
  match previous_status with
  | Up since -> if (can_ping "lo" "localhost") then Up since else Down (now ())
  | Down since -> if (can_ping "lo" "localhost") then Up (now ()) else Down since
  | Waiting since -> if ((now ()) - since) > boot_duration then get_modem_status boot_duration (Down since) else Waiting since


let get_internet_status previous_status =
  let could_curl = (can_curl "wlp3s0" "10" "google.com") in
  match previous_status with
  | Up since -> if could_curl then Up since else Down (now ())
  | Down since -> if could_curl then Up (now ()) else Down since
  | Waiting since -> Down since (* We wait for the modem, not the Internet. Note that we never construct this state for the Internet; could use different types for Modem & Internet *)


let rec mainloop program_state modem_status internet_status =

  let () = Unix.sleep 1 in
  
  match program_state with

  | MonitoringModem ->
      print_endline "Monitoring modem";
      (let new_modem_status = get_modem_status 5 modem_status in
       match new_modem_status with
       | Waiting _ -> mainloop MonitoringModem new_modem_status internet_status
       | Down _ -> mainloop MonitoringModem new_modem_status internet_status
       | Up   _ -> mainloop MonitoringInternet new_modem_status (Down (now ())))

  | MonitoringInternet ->
      print_endline "Monitoring internet";
      (let new_internet_status = get_internet_status internet_status in
       match new_internet_status with
       | Waiting _ -> mainloop MonitoringInternet modem_status new_internet_status
       | Down since -> if ((now ()) - since) > 5 then mainloop RebootingModem modem_status internet_status else mainloop MonitoringInternet modem_status new_internet_status
       | Up   _ -> mainloop MonitoringInternet modem_status new_internet_status)

  | RebootingModem ->
      print_endline "Rebooting modem";
      mainloop MonitoringModem (Waiting (now ())) (Down (now ()))


let () =
  mainloop MonitoringModem (Waiting (now ())) (Down (now ()))











(* Testing: *)
      
(* let () =
 *   match (get_modem_status 120 (Down 0)) with
 *   | Waiting since -> print_endline ("waiting" ^ string_of_int since)
 *   | Down since -> print_endline ("down " ^ string_of_int since)
 *   | Up   since -> print_endline ("up " ^ string_of_int since)
 * 
 * 
 * let () =
 *   match (get_internet_status (Down 0)) with
 *   | Waiting since -> print_endline ("waiting" ^ string_of_int since)
 *   | Down since -> print_endline ("down " ^ string_of_int since)
 *   | Up   since -> print_endline ("up " ^ string_of_int since)
 * 
 * 
 * let () = can_ping "wlp3s0" "google.com" |> string_of_bool |> print_endline
 * let () = can_curl "wlp3s0" "10" "google.com" |> string_of_bool |> print_endline *)

  
