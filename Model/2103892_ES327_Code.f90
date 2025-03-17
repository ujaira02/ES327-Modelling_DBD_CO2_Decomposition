program CO2modelling
    implicit none

    real(8), parameter :: R = 8.31446261815324
    real(8), parameter :: epsilon_0 = 8.85418782E-12
    real(8), parameter :: kB = 1.380649E-23
    real(8), parameter :: pi = acos(-1.0)
    real(8), parameter :: n_AV = 6.02214076E23
    real(8), parameter :: Evib = 0.291 * 1.60218E-19
    real(8), parameter :: Ee_avg = 5.09003 * 1.60218E-19
    
    real(8), parameter :: dt = 1E-9
    integer, parameter :: n = 5E4
     
    real(8), parameter :: diam_reac = 23E-3
    real(8), parameter :: b_pyr = 1E-3
    real(8), parameter :: h_pyr = 1E-3
    integer, parameter :: n_pyr = 160
    
    real(8), parameter :: m_mol = 4.4009E-2
    real(8), parameter :: V_mol = 2.24E-2
    
    real(8), parameter :: c_pCO2 = 844.9
    real(8), parameter :: c_pO2 = 910.7
    real(8), parameter :: c_pCO = 1049.0
    real(8), parameter :: c_pO = 1369.5
    
    real(8), parameter :: h = 0.658
    real(8), parameter :: epsilon_gap = 1.000984 * 8.85418782E-12

    real(8), parameter :: v_discharge = 5.2E3
    real(8), parameter :: eff_discharge = 0.2
    real(8), parameter :: tau_vt = 1E-7
    real(8), parameter :: T0 = 298.15

    real(8), parameter :: d_dielectric = 200E-6
    real(8), parameter :: epsilon_dielectric = 5 * 8.85418782E-12
 
    integer, parameter :: reactions = 30
    real(8), dimension(reactions) :: A_f(reactions), A_b(reactions), Ea_f(reactions), Ea_b(reactions), k_f(reactions), k_b(reactions) 

    integer :: i, j 
    
    real(8) :: t    
    real(8) :: Patm, v_AC, f_AC
    real(8) :: d_gap, V, A  
    real(8) :: Q, n_dot, n_mol, n_CO2, m_kg, N_gas, m_dot   
    real(8) :: Tgas, Texc
    real(8) :: ne, kexc
    real(8) :: CO2, CO, C, C2, O, O2, O3, CO2_plus, CO_plus, O3_plus, O2_plus, O_plus, O2_minus, O_minus, CO2_exc, O2_exc, O_ad, M, S, e
    real(8) :: P_plasma, E_N, v_input, i_input, SEI, dv_input, t_discharge
    real(8) :: k
    character(len=20) :: input_str
    integer :: choice1, choice2, status, istat
    logical :: is_valid
    
    character(len=20) :: filename
    integer :: target_row, target_column

    real(8), parameter :: alpha = 1.8E3
    real(8), parameter :: beta = 0

    Tgas = 298.15
    Texc = 298.15
    
    CO2 = n_CO2  
    CO = 0
    C = 0
    C2 = 0
    O = 0
    O2 = 0
    O3 = 0
    CO2_plus = 0
    CO_plus = 0
    O3_plus = 0
    O2_plus = 0
    O_plus = 0
    O2_minus = 0
    O_minus = 0
    CO2_exc = 0
    O2_exc = 0
    O_ad = 0
    M = 0
    S = 0
    e = 0
    
    A_f = (/ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0E12, 1.0E11, 1.0E8, 1.0E8, 1.0E12, 1.0E9, 1.0E10, 1.0E11, 1.0E10, 1.0E10, 1.0E11, 1.0E11, 0.0, 0.0, 0.0, 1.0E11, 1.0E10, 1.0E10, 1.0E11, 0.0, 0.0 /)
    A_b = (/ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0E9, 1.0E10, 1.0E12, 1.0E9, 1.0E12, 1.0E10, 1.0E10, 1.0E11, 1.0E10, 1.0E10, 1.0E10, 1.0E11, 0.0, 0.0, 0.0, 1.0E11, 1.0E10, 1.0E10, 1.0E11, 0.0, 0.0 /)
    Ea_f = 1E3 * (/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 275, 225, 5, 5, 325, 125, 30, 75, 40, 30, 5, 5, 0, 0, 0, 75, 125, 175, 225, 0, 0 /)
    Ea_b = 1E3 * (/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 75, 325, 450, 125, 325, 150, 150, 75, 40, 75, 125, 175, 0, 0, 0, 325, 150, 200, 250, 0, 0 /)
  
    filename = 'CO2ratesF.dat'
    
    print *, "======================================================================================="
    print *, "======================================================================================="
    print *, "                                CO2 Decomposition Model                                "
    print *, "======================================================================================="
    print *, "======================================================================================="
    print *, " 1. Run Simulation                       "
    print *, " 2. Display Results      [COMING SOON]   "
    print *, " 3. Optimise Parameters  [COMING SOON]   "
    print *, " 4. Exit                                 "
    print *, "======================================================================================="

    do
        do
            is_valid = .true.
            print *, "Enter your choice: "
            read(*, '(A)') input_str
            do i = 1, len_trim(input_str)
                if (input_str(i:i) < '0' .or. input_str(i:i) > '9') then
                    is_valid = .false.
                    exit
                end if
            end do
            if (is_valid) then
                read(input_str, '(I10)', IOSTAT=status) choice1
                if (status == 0 .and. choice1 > 0) then
                    exit
                end if
            end if
            print *, "Invalid choice. Please enter a natural number (1, 2, 3, ...). Please try again."
            print *, "                                                             "
        end do
        if (choice1 == 1) then
            exit
        else if (choice1 == 4) then
            stop
        else
            print *, "Invalid choice. Please try again."
            print *, "                                 "
        end if
    end do
    
    print *, "                                                                                       "
    print *, "======================================================================================="
    print *, "                                   Input Parameters                                    "
    print *, "======================================================================================="
    print *, " 1. Optimised Parameters                                                               "
    print *, " 2. Custom Parameters                                                                  "
    print *, " 3. Exit                                                                               "
    print *, "======================================================================================="
    
    do
        do
            is_valid = .true.
            print *, "Enter your choice: "
            read(*, '(A)') input_str
            do i = 1, len_trim(input_str)
                if (input_str(i:i) < '0' .or. input_str(i:i) > '9') then
                    is_valid = .false.
                    exit
                end if
            end do
            if (is_valid) then
                read(input_str, '(I10)', IOSTAT=status) choice2
                if (status == 0 .and. choice2 > 0) then
                   exit
                end if
            end if
            print *, "Invalid choice. Please enter a natural number (1, 2, 3, ...). Please try again."
            print *, "                                                             "
        end do
        if (choice2 == 1) then
            Patm = 101325
            v_AC = 6500
            f_AC = 68000
            Q = 20
            d_gap = 0.25
            exit
        else if (choice2 == 2) then
            exit
        else if (choice2 == 3) then
            stop
        else
            print *, "Invalid choice. Please try again."
            print *, "                                 "
        end if
            print *, "                                 "
    end do
    if (choice2 == 2) then
        do
            print *, "Enter reactor chamber pressure [atm]:"
            read(*,*,iostat=istat) Patm
            
            if (istat == 0 .and. Patm >= 0.5 .and. Patm <= 1.5) then
                exit
            else
                print *, "Invalid input. Input must be a real number in the range [0.5, 1.5]. Please try again."
            end if
        end do
        
        do
            print *, "Enter AC input voltage [V]:"
            read(*,*,iostat=istat) v_AC
            
            if (istat == 0 .and. v_AC >= 0 .and. v_AC <= 10000) then
                exit
            else
                print *, "Invalid input. Input must be a real number in the range [0, 10,000]. Please try again."
            end if
        end do
        
        do
            print *, "Enter AC input frequency [Hz]:"
            read(*,*,iostat=istat) f_AC
            
            if (istat == 0 .and. f_AC >= 0 .and. f_AC <= 100000) then
                exit
            else
                print *, "Invalid input. Input must be a real number in the range [0, 100,000]. Please try again."
            end if
        end do
        
        do
            print *, "Enter volumetric flow rate [mL/min]:"
            read(*,*,iostat=istat) Q
            
            if (istat == 0 .and. Q >= 0 .and. Q <= 1000) then
                exit
            else
                print *, "Invalid input. Input must be a real number in the range [0, 1,000]. Please try again."
            end if
        end do
        
        do
            print *, "Enter electrode gap [mm]:"
            read(*,*,iostat=istat) d_gap
            
            if (istat == 0 .and. d_gap >= 0 .and. d_gap <= 1) then
                exit
            else
                print *, "Invalid input. Input must be a real number in the range [0, 1]. Please try again."
            end if
        end do
    end if
    Patm = Patm * 101325
    
    Q = Q * (1E-6 / 60)
    n_dot = Q / V_mol
    m_dot = n_dot * m_mol 
    
    d_gap = d_gap * 1E-3
    V = pi * (diam_reac/2)**2 * (d_gap + h_pyr) - n_pyr * ((b_pyr**2 * h_pyr) / 3)
    A = pi * (diam_reac/2)**2 + n_pyr * (4 * (0.5 * ((b_pyr**2 + h_pyr**2)**0.5) * b_pyr) - b_pyr**2)

    m_kg = (Patm * V * m_mol) / (R * Tgas)
    n_mol = V / V_mol
    n_CO2 = n_mol * n_AV
    
    N_gas = Patm / (kB * Tgas)
    ne = Patm / (kB * Texc)
    kexc = 0.426196E-15
     
    print *, "                                                                                       "
    print *, "======================================================================================="
    print *, "                                 Simulation running...                                 "  

    open(10, file='plotdata.dat')
        do i = 1, n
            t = i * dt

            CO2 = n_CO2  
            CO = 0
            C = 0
            C2 = 0
            O = 0
            O2 = 0
            O3 = 0
            CO2_plus = 0
            CO_plus = 0
            O3_plus = 0
            O2_plus = 0
            O_plus = 0
            O2_minus = 0
            O_minus = 0
            CO2_exc = 0
            O2_exc = 0
            O_ad = 0
            M = 0
            S = 0
            e = 0  

                call plasma_power(v_AC, v_discharge, f_AC, d_gap, epsilon_0, epsilon_dielectric, epsilon_gap, A, t, eff_discharge, E_N, P_plasma, pi, v_input, i_input, dv_input, SEI, n_dot, N_gas, t_discharge)
                call temperature_dynamics(Tgas, dt, A, P_plasma, c_pCO2, c_pCO, c_pO2, c_pO, h, CO2, CO, O2, O, n_CO2, m_dot, T0)
                call excited_dynamics(Texc, dt, ne, Evib, Ee_avg, Tgas, kexc, tau_vt, kB)
                call arrhenius_rates(R, Tgas, A_f, A_b, Ea_f, Ea_b, k_f, k_b, j, reactions)
                call bolsig_row(filename, E_N, target_row)

                    filename = 'CO2ratesF.dat'
                    target_column = 4
                    call bolsig_rates(filename, target_row, target_column, k)
                    k_f(1) = k
                    target_column = 16
                    call bolsig_rates(filename, target_row, target_column, k)
                    k_f(2) = k
                    target_column = 4
                    call bolsig_rates(filename, target_row, target_column, k)
                    k_f(3) = k
                    target_column = 12
                    call bolsig_rates(filename, target_row, target_column, k)
                    k_f(4) = k
                    target_column = 4
                    call bolsig_rates(filename, target_row, target_column, k)
                    k_f(29) = k
                    target_column = 4
                    call bolsig_rates(filename, target_row, target_column, k)  
                    k_f(30) = k
                    filename = 'O2ratesF.dat'
                    target_column = 16
                    call bolsig_rates(filename, target_row, target_column, k)
                    k_f(5) = k
                    target_column = 20
                    call bolsig_rates(filename, target_row, target_column, k)
                    k_f(6) = k
                    target_column = 5
                    call bolsig_rates(filename, target_row, target_column, k)
                    k_f(7) = k
                    target_column = 20
                    call bolsig_rates(filename, target_row, target_column, k)
                    k_f(8) = k
                    target_column = 4
                    call bolsig_rates(filename, target_row, target_column, k)
                    k_f(9) = k
                    k_b(1) = 0
                    k_b(2) = 0
                    k_b(3) = 0
                    k_b(6) = 0
                    k_b(7) = 0
                    k_b(8) = 0
                    k_b(9) = 0
                    k_b(29) = 0
                    k_b(30) = 0
                    filename = 'CO2ratesB.dat'
                    target_column = 13
                    call bolsig_rates(filename, target_row, target_column, k)
                    k_b(4) = k
                    filename = 'O2ratesB.dat'
                    target_column = 16
                    call bolsig_rates(filename, target_row, target_column, k)
                    k_b(5) = k
                    
                    k_f = alpha * k_f
                    k_b = beta * k_b
                    
                    if (v_input > 0) then
                        if (abs(v_input) > ((0.5 * v_discharge) - 0.5)) then
                            if (abs(v_input) < ((0.5 * v_discharge) + 0.5)) then
                                if (dv_input > 0) then
                                    Tgas = 298.15
                                    Texc = 3500
                                
                                    CO2 = n_CO2
                                    
                                    call kinetics_step(CO2, CO, C, C2, O, O2, O3, CO2_plus, CO_plus, O3_plus, O2_plus, O_plus, O2_minus, O_minus, CO2_exc, O2_exc, O_ad, M, S, e, k_f, k_b, t, t_discharge, dt, reactions, n_CO2)
                                end if
                            end if
                        end if
                    else if (v_input < 0) then
                        if (abs(v_input) > ((0.5 * v_discharge) - 0.5)) then
                            if (abs(v_input) < ((0.5 * v_discharge) + 0.5)) then
                                if (dv_input < 0) then
                                    Tgas = 298.15
                                    Texc = 3500
                                
                                    CO2 = n_CO2
                                    
                                    call kinetics_step(CO2, CO, C, C2, O, O2, O3, CO2_plus, CO_plus, O3_plus, O2_plus, O_plus, O2_minus, O_minus, CO2_exc, O2_exc, O_ad, M, S, e, k_f, k_b, t, t_discharge, dt, reactions, n_CO2)
                                end if
                            end if
                        end if
                    end if
                
                call kinetics_step(CO2, CO, C, C2, O, O2, O3, CO2_plus, CO_plus, O3_plus, O2_plus, O_plus, O2_minus, O_minus, CO2_exc, O2_exc, O_ad, M, S, e, k_f, k_b, t, t_discharge, dt, reactions, n_CO2)

                if (t <= t_discharge) then
                    CO2 = n_CO2  
                    CO = 0
                    C = 0
                    C2 = 0
                    O = 0
                    O2 = 0
                    O3 = 0
                    CO2_plus = 0
                    CO_plus = 0
                    O3_plus = 0
                    O2_plus = 0
                    O_plus = 0
                    O2_minus = 0
                    O_minus = 0
                    CO2_exc = 0
                    O2_exc = 0
                    O_ad = 0
                    M = 0
                    S = 0
                    e = 0
                    
                    E_N = 0
                end if

            write(10,*) t, E_N, v_input, i_input, P_plasma, SEI, Tgas, Texc, CO2 / n_CO2, CO / n_CO2, C / n_CO2, C2 / n_CO2, O / n_CO2, O2 / n_CO2, O3 / n_CO2, CO2_plus / n_CO2, CO_plus / n_CO2, O3_plus / n_CO2, O2_plus / n_CO2, O_plus / n_CO2, O2_minus / n_CO2, O_minus / n_CO2, CO2_exc / n_CO2, O2_exc / n_CO2, O_ad / n_CO2, M / n_CO2, S / n_CO2, e / n_CO2
        end do
    close(10)
    print *, "                                 Simulation complete.                                  "
    print *, "                              Data saved to plotdata.dat.                              "
    print *, "======================================================================================="
    print *, "           -----------------------------------------------------------------           "
    print *, "           Column   Variable                  Units  |   Column   Variable             "
    print *, "           ----------------------------------------  |  --------------------           "
    print *, "              1     Time                      [s]    |      9     CO2                  "
    print *, "              2     Reduced Electric Field    [Td]   |     10     CO                   "
    print *, "              3     Input Voltage             [V]    |     11     C                    "
    print *, "              4     Input Ccurrent            [A]    |     12     C2                   "
    print *, "              5     Plasma Power Inpu         [W]    |     13     O                    "
    print *, "              6     Specific Energy Input     [J]    |     15     O3                   "
    print *, "              7     Bulk Gas Temperature      [K]    |     14     O2                   "
    print *, "              8     Excited Gas Temperature   [K]    |     16     CO2+                 "
    print *, "                                                     |     17     CO+                  "
    print *, "                                                     |     18     O3+                  "
    print *, "                                                     |     19     O2+                  "
    print *, "                                                     |     20     O+                   "
    print *, "                                                     |     21     O2-                  "
    print *, "                                                     |     22     O-                   "
    print *, "                                                     |     23     CO2*                 "
    print *, "                                                     |     24     O2*                  "
    print *, "                                                     |     25     O_ad                 "
    print *, "                                                     |     26     M                    "
    print *, "                                                     |     27     S                    "
    print *, "                                                     |     28     e-                   "
    print *, "           -----------------------------------------------------------------           "     
    print *, "======================================================================================="
    print *, "                                                                                       "
    print *, "                                                                                       "
    print *, "                                                                                       "     
    print *, "                                                                                       "
    print *, "                                                                                       "

end program CO2modelling

subroutine plasma_power(v_AC, v_discharge, f_AC, d_gap, epsilon_0, epsilon_dielectric, epsilon_gap, A, t, eff_discharge, E_N, P_plasma, pi, v_input, i_input, dv_input, SEI, n_dot, N_gas, t_discharge)
    implicit none

    real(8), intent(in) :: t, pi, d_gap, A, N_gas, n_dot, v_AC, v_discharge, eff_discharge, f_AC, epsilon_0, epsilon_dielectric, epsilon_gap
    real(8), intent(out) :: E_N, v_input, i_input, P_plasma, SEI, dv_input, t_discharge
    real(8) :: C_gap, C_dielectric, C_input, v_gap, v_dielectric

    C_gap = (epsilon_0 * epsilon_gap * A)/ d_gap
    C_dielectric = (epsilon_0 * epsilon_dielectric * A) / d_gap
    C_input = (epsilon_0 * epsilon_dielectric * epsilon_gap * A) / (d_gap * (epsilon_gap + epsilon_dielectric))

    v_input = (v_AC / 2) * sin(2 * pi * f_AC * t)
    dv_input = (v_AC / 2) * 2 * pi * f_AC * cos(2 * pi * f_AC * t)
    t_discharge = (1 / (2 * pi * f_AC)) * asin(2 * ((v_discharge / 2) / v_AC))
    v_gap = ((v_AC / 2) * sin((2 * pi * f_AC) * (t - t_discharge))) * (C_dielectric / (C_gap + C_dielectric))
    v_dielectric = v_input * (C_gap / (C_gap + C_dielectric))
    
    E_N = abs(((v_gap / d_gap) / N_gas) * 1E21)
    
    if (abs(v_input) > (0.5 * v_discharge)) then
        i_input = C_input * v_AC * f_AC * pi
    else
        i_input = 0
    end if

    P_plasma = abs(v_input * i_input * eff_discharge)
    SEI = P_plasma / n_dot
end subroutine plasma_power

subroutine temperature_dynamics(Tgas, dt, A, P_plasma, c_pCO2, c_pCO, c_pO2, c_pO, h, CO2, CO, O2, O, n_CO2, m_dot, T0)
    implicit none

    real(8), intent(inout) :: Tgas
    real(8), intent(in) :: dt, A, P_plasma, c_pCO2, c_pCO, c_pO2, c_pO, h, CO2, CO, O2, O, n_CO2, m_dot, T0
    real(8) :: c_p

    c_p = (CO2 / n_CO2) * (c_pCO2) + (CO / n_CO2) * (c_pCO) + (O2 / n_CO2) * (c_pO2) + (O / n_CO2) * (c_pO)
    Tgas = Tgas + dt * ((P_plasma - (h * A) * (Tgas - T0)) / (m_dot * c_p))
end subroutine temperature_dynamics

subroutine excited_dynamics(Texc, dt, ne, Evib, Ee_avg, Tgas, kexc, tau_vt, kB)
    implicit none

    real(8), intent(inout) :: Texc 
    real(8), intent(in) :: dt, ne, Evib, Ee_avg, kexc, tau_vt, kB, Tgas
    real(8) :: E_gain, E_loss

    E_gain = kexc * ne * Ee_avg * Evib / kB
    E_loss = (Texc - Tgas) / tau_vt

    Texc = Texc + (E_gain - E_loss) * dt
    if (Texc < Tgas) Texc = Tgas    
end subroutine excited_dynamics

subroutine bolsig_row(filename, E_N, target_row)
    implicit none

    character(len=*), intent(in) :: filename
    real(8), intent(in) :: E_N
    integer, intent(out) :: target_row
    character(len=300) :: line
    integer :: iostat, current_row
    real(8) :: current_value, target_EN, diff, min_diff
    real(8), allocatable :: data_row(:)

    target_EN = 0.0
    min_diff = huge(0.0)
    target_row = -1
    current_row = 0

    open(unit=20, file=filename, status="old", action="read")

    do
        read(20, '(A)', iostat=iostat) line
        if (iostat /= 0) exit

        current_row = current_row + 1

        allocate(data_row(count_columns(line)))
        call line_to_array(line, data_row)

        if (size(data_row) >= 2) then
            current_value = data_row(2)
            diff = abs(current_value - E_N)

            if (diff < min_diff) then
                min_diff = diff
                target_EN = current_value
                target_row = current_row
            end if
        end if

        deallocate(data_row)
    end do

    close(20)

    if (target_row <= 0) then
        print *, "Error: No valid data found in the file. Please try again"
        stop
    end if

contains

    integer function count_columns(input_line)
        character(len=*), intent(in) :: input_line
        integer :: i, n
        logical :: in_word

        n = 0
        in_word = .false.
        do i = 1, len_trim(input_line)
            if (input_line(i:i) /= ' ' .and. .not. in_word) then
                n = n + 1
                in_word = .true.
            else if (input_line(i:i) == ' ') then
                in_word = .false.
            end if
        end do
        count_columns = n
    end function count_columns

    subroutine line_to_array(input_line, array)
        character(len=*), intent(in) :: input_line
        real(8), allocatable, intent(out) :: array(:)
        integer :: i, local_n_columns

        local_n_columns = count_columns(input_line)
        allocate(array(local_n_columns))
        read(input_line, *) (array(i), i = 1, local_n_columns)
    end subroutine line_to_array
end subroutine bolsig_row

subroutine arrhenius_rates(R, Tgas, A_f, A_b, Ea_f, Ea_b, k_f, k_b, j, reactions)
    implicit none

    integer :: j, reactions
    real(8), intent(in) :: R, Tgas
    real(8), dimension(reactions), intent(in) :: A_f, A_b, Ea_f, Ea_b
    real(8), dimension(reactions), intent(out) :: k_f, k_b
    do j = 1, reactions
        k_f(j) = A_f(j) * exp(-Ea_f(j) / (R * Tgas))
        k_b(j) = A_b(j) * exp(-Ea_b(j) / (R * Tgas))
    end do
    
end subroutine arrhenius_rates

subroutine bolsig_rates(filename, target_row, target_column, k)
    implicit none
   
    character(len=*), intent(in) :: filename
    integer, intent(in) :: target_row, target_column
    real(8), intent(out) :: k

    integer :: iostat, current_row
    character(len=300) :: line
    real(8), allocatable :: data_row(:)
    integer :: n_columns

    current_row = 0
    k = 0.0

    open(unit=30, file=filename, status="old", action="read")

    do
        read(30, '(A)', iostat=iostat) line
        if (iostat /= 0) exit

        current_row = current_row + 1

        if (current_row == target_row) then
            n_columns = count_columns(line)
            allocate(data_row(n_columns))
            call line_to_array(line, data_row)

            if (size(data_row) >= target_column) then
                k = data_row(target_column)
            else
                print *, "Error: Target column out of range."
            end if

            deallocate(data_row)
            exit
        end if
    end do

    close(30)

contains

    integer function count_columns(input_line)
        character(len=*), intent(in) :: input_line
        integer :: i, n
        logical :: in_word

        n = 0
        in_word = .false.
        do i = 1, len_trim(input_line)
            if (input_line(i:i) /= ' ' .and. .not. in_word) then
                n = n + 1
                in_word = .true.
            else if (input_line(i:i) == ' ') then
                in_word = .false.
            end if
        end do
        count_columns = n
    end function count_columns

    subroutine line_to_array(input_line, array)
        character(len=*), intent(in) :: input_line
        real(8), allocatable, intent(out) :: array(:)
        integer :: i, local_n_columns

        local_n_columns = count_columns(input_line)
        allocate(array(local_n_columns))
        read(input_line, *) (array(i), i = 1, local_n_columns)
    end subroutine line_to_array

end subroutine bolsig_rates

subroutine kinetics_step(CO2, CO, C, C2, O, O2, O3, CO2_plus, CO_plus, O3_plus, O2_plus, O_plus, O2_minus, O_minus, CO2_exc, O2_exc, O_ad, M, S, e, k_f, k_b, t, t_discharge, dt, reactions, n_CO2)
    implicit none

    real(8), intent(inout) :: CO2, CO, C, C2, O, O2, O3, CO2_plus, CO_plus, O3_plus, O2_plus, O2_minus, O_plus, O_minus, CO2_exc, O2_exc, O_ad, M, S, e
    real(8), intent(in) :: t, t_discharge, dt, n_CO2
    integer, intent(in) :: reactions
    real(8), dimension(reactions) :: k_f, k_b
    real(8) :: dCO2_1, dCO_1, dC_1, dC2_1, dO_1, dO2_1, dO3_1, dCO2_plus_1, dCO_plus_1, dO3_plus_1, dO2_plus_1, dO_plus_1, dO2_minus_1, dO_minus_1, dCO2_exc_1, dO2_exc_1, dO_ad_1, dM_1, dS_1, de_1
    real(8) :: dCO2_2, dCO_2, dC_2, dC2_2, dO_2, dO2_2, dO3_2, dCO2_plus_2, dCO_plus_2, dO3_plus_2, dO2_plus_2, dO_plus_2, dO2_minus_2, dO_minus_2, dCO2_exc_2, dO2_exc_2, dO_ad_2, dM_2, dS_2, de_2
    real(8) :: dCO2_3, dCO_3, dC_3, dC2_3, dO_3, dO2_3, dO3_3, dCO2_plus_3, dCO_plus_3, dO3_plus_3, dO2_plus_3, dO_plus_3, dO2_minus_3, dO_minus_3, dCO2_exc_3, dO2_exc_3, dO_ad_3, dM_3, dS_3, de_3
    real(8) :: dCO2_4, dCO_4, dC_4, dC2_4, dO_4, dO2_4, dO3_4, dCO2_plus_4, dCO_plus_4, dO3_plus_4, dO2_plus_4, dO_plus_4, dO2_minus_4, dO_minus_4, dCO2_exc_4, dO2_exc_4, dO_ad_4, dM_4, dS_4, de_4
    real(8) :: CO2_temp, CO_temp, C_temp, C2_temp, O_temp, O2_temp, O3_temp, CO2_plus_temp, CO_plus_temp, O3_plus_temp, O2_plus_temp, O2_minus_temp, O_plus_temp, O_minus_temp, CO2_exc_temp, O2_exc_temp, O_ad_temp, M_temp, S_temp, e_temp
    real(8) :: CO2_old, CO_old, C_old, C2_old, O_old, O2_old, O3_old, CO2_plus_old, CO_plus_old, O3_plus_old, O2_plus_old, O2_minus_old, O_plus_old, O_minus_old, CO2_exc_old, O2_exc_old, O_ad_old, M_old, S_old, e_old

    CO2_old = CO2
    CO_old = CO
    C_old = C
    C2_old = C2
    O_old = O
    O2_old = O2
    O3_old = O3
    CO2_plus_old = CO2_plus
    CO_plus_old = CO_plus
    O3_plus_old = O3_plus
    O2_plus_old = O2_plus
    O2_minus_old = O2_minus
    O_plus_old = O_plus
    O_minus_old = O_minus
    CO2_exc_old = CO2_exc
    O2_exc_old = O2_exc
    O_ad_old = O_ad
    M_old = M
    S_old = S
    e_old = e

    dCO2_1 = -2 * CO2**2 * k_f(14) - CO2 * CO2_exc * k_f(30) - CO2 * O2 * k_b(15) - CO2 * O2_plus * k_b(18) - CO2 * O * k_b(21) - CO2 * O * k_f(10) - CO2 * e * k_f(1) - CO2 * e * k_f(2) - CO2 * e * k_f(3) - CO2 * e * k_f(4) + CO2_exc * e * k_b(4) + CO2_plus * O2 * k_f(18) + CO2_plus * O_minus * k_f(21) + CO2_plus * e**2 * k_b(2) + 2 * CO**2 * O2 * k_b(14) + CO**2 * O2 * k_b(30) + CO * O2 * k_b(10) + CO * O * e * k_b(1) + CO_plus * O * e**2 * k_b(3) + O3 * O * k_f(15)
    dCO_1 = 2 * C2 * O2 * k_f(28) + C2 * O * k_b(27) + 2 * CO2**2 * k_f(14) + 2 * CO2 * CO2_exc * k_f(30) + CO2 * CO2_plus * k_f(17) - CO2 * CO * O_plus * k_b(17) + CO2 * O * k_f(10) + CO2 * e * k_f(1) + CO2_exc * k_f(29) - 2 * CO**2 * O2 * k_b(14) - 2 * CO**2 * O2 * k_b(30) - 2 * CO**2 * k_b(28) - CO * C * k_f(27) - CO * O2 * k_b(10) - CO * O * e * k_b(1) - CO * O * k_b(29) - CO * O * k_f(11) - CO * k_b(25) + C * O2 * k_b(11) + C * O * k_f(25)
    dC_1 = C2 * O * k_b(27) + 2 * C2 * k_b(26) - CO * C * k_f(27) + CO * O * k_f(11) + CO * k_b(25) - 2 * C**2 * k_f(26) - C * O2 * k_b(11) - C * O * k_f(25)
    dC2_1 = -C2 * O2 * k_f(28) - C2 * O * k_b(27) - C2 * k_b(26) + CO**2 * k_b(28) + CO * C * k_f(27) + C**2 * k_f(26)
    dO_1 = -C2 * O * k_b(27) + CO2 * O2 * k_b(15) - CO2 * O * k_b(21) - CO2 * O * k_f(10) + CO2 * e * k_f(1) + CO2 * e * k_f(3) + CO2_exc * k_f(29) + CO2_plus * O_minus * k_f(21) + CO * C * k_f(27) + CO * O2 * k_b(10) - CO * O * e * k_b(1) - CO * O * k_b(29) - CO * O * k_f(11) + CO * k_b(25) - CO_plus * O * e**2 * k_b(3) + C * O2 * k_b(11) - C * O * k_f(25) - M * O2 * O * k_f(13) + 2 * M * O2 * k_b(12) + M * O3 * k_b(13) - 2 * M * O**2 * k_f(12) - M * O * e * k_f(9) + M * O_minus * k_b(9) + O2**2 * k_b(16) + O2 * O2_plus * k_f(19) - O2 * O * k_b(20) + O2 * e * k_f(7) - O2_minus * O * k_b(7) + O2_minus * O_plus * k_f(20) - O3 * O * k_f(15) - O3 * O * k_f(16) - O3_plus * O * k_b(19) - O * S * k_f(22) - O * e * k_f(8) + O_ad * k_b(22) + O_minus * e * k_b(8)
    dO2_1 = -C2 * O2 * k_f(28) + CO2**2 * k_f(14) + CO2 * CO2_exc * k_f(30) - CO2 * O2 * k_b(15) + CO2 * O2_plus * k_b(18) + CO2 * O * k_f(10) - CO2_plus * O2 * k_f(18) - CO**2 * O2 * k_b(14) - CO**2 * O2 * k_b(30) + CO**2 * k_b(28) - CO * O2 * k_b(10) + CO * O * k_f(11) - C * O2 * k_b(11) - M * O2 * O * k_f(13) - M * O2 * k_b(12) + M * O3 * k_b(13) + M * O**2 * k_f(12) - 2 * O2**2 * k_b(16) - O2 * O2_plus * k_f(19) - O2 * O * k_b(20) - O2 * O_ad * k_b(24) - O2 * e * k_f(5) - O2 * e * k_f(6) - O2 * e * k_f(7) - O2 * k_b(23) + O2_exc * e * k_b(5) + O2_minus * O * k_b(7) + O2_minus * O_plus * k_f(20) + O2_plus * e**2 * k_b(6) + O3 * O * k_f(15) + 2 * O3 * O * k_f(16) + O3 * S * k_f(24) + O3_plus * O * k_b(19) + O_ad**2 * k_f(23)
    dO3_1 = CO2 * O2 * k_b(15) + M * O2 * O * k_f(13) - M * O3 * k_b(13) + O2**2 * k_b(16) + O2 * O_ad * k_b(24) - O3 * O * k_f(15) - O3 * O * k_f(16) - O3 * S * k_f(24)
    dCO2_plus_1 = -CO2 * CO2_plus * k_f(17) + CO2 * CO * O_plus * k_b(17) + CO2 * O2_plus * k_b(18) + CO2 * O * k_b(21) + CO2 * e * k_f(2) - CO2_plus * O2 * k_f(18) - CO2_plus * O_minus * k_f(21) - CO2_plus * e**2 * k_b(2)
    dCO_plus_1 = CO2 * e * k_f(3) - CO_plus * O * e**2 * k_b(3)
    dO3_plus_1 = O2 * O2_plus * k_f(19) - O3_plus * O * k_b(19)
    dO2_plus_1 = -CO2 * O2_plus * k_b(18) + CO2_plus * O2 * k_f(18) - O2 * O2_plus * k_f(19) + O2 * e * k_f(6) - O2_plus * e**2 * k_b(6) + O3_plus * O * k_b(19)
    dO_plus_1 = CO2 * CO2_plus * k_f(17) - CO2 * CO * O_plus * k_b(17) + O2 * O * k_b(20) - O2_minus * O_plus * k_f(20)
    dO2_minus_1 = O2 * O * k_b(20) - O2_minus * O_plus * k_f(20) + O2 * e * k_f(7) + O2 * e * k_f(5)
    dO_minus_1 = CO2 * O * k_b(21) - CO2_plus * O_minus * k_f(21) + M * O * e * k_f(9) - M * O_minus * k_b(9) + O * e * k_f(8) - O_minus * e * k_b(8)
    dCO2_exc_1 = -CO2 * CO2_exc * k_f(30) + CO2 * e * k_f(4) - CO2_exc * e * k_b(4) - CO2_exc * k_f(29) + CO**2 * O2 * k_b(30) + CO * O * k_b(29)
    dO2_exc_1 = O2 * e * k_f(5) - O2_exc * e * k_b(5)
    dO_ad_1 = -O2 * O_ad * k_b(24) + 2 * O2 * k_b(23) + O3 * S * k_f(24) + O * S * k_f(22) - 2 * O_ad**2 * k_f(23) - O_ad * k_b(22)
    dM_1 = 0
    dS_1 = O2 * O_ad * k_b(24) - O3 * S * k_f(24) - O * S * k_f(22) + O_ad * k_b(22)
    de_1 = CO2 * e * k_f(2) + CO2 * e * k_f(3) - CO2_plus * e**2 * k_b(2) - CO_plus * O * e**2 * k_b(3) - M * O * e * k_f(9) + M * O_minus * k_b(9) + O2 * e * k_f(6) - O2 * e * k_f(7) + O2_minus * O * k_b(7) - O2_plus * e**2 * k_b(6)

    CO2_temp = CO2 + 0.5 * dCO2_1 * dt
    CO_temp = CO + 0.5 * dCO_1 * dt
    C_temp = C + 0.5 * dC_1 * dt
    C2_temp = C2 + 0.5 * dC2_1 * dt
    O_temp = O + 0.5 * dO_1 * dt
    O2_temp = O2 + 0.5 * dO2_1 * dt
    O3_temp = O3 + 0.5 * dO3_1 * dt
    CO2_plus_temp = CO2_plus + 0.5 * dCO2_plus_1 * dt
    CO_plus_temp = CO_plus + 0.5 * dCO_plus_1 * dt
    O3_plus_temp = O3_plus + 0.5 * dO3_plus_1 * dt
    O2_plus_temp = O2_plus + 0.5 * dO2_plus_1 * dt
    O_plus_temp = O_plus + 0.5 * dO_plus_1 * dt
    O2_minus_temp = O2_minus + 0.5 * dO2_minus_1 * dt
    O_minus_temp = O_minus + 0.5 * dO_minus_1 * dt
    CO2_exc_temp = CO2_exc + 0.5 * dCO2_exc_1 * dt
    O2_exc_temp = O2_exc + 0.5 * dO2_exc_1 * dt
    O_ad_temp = O_ad + 0.5 * dO_ad_1 * dt
    M_temp = M + 0.5 * dM_1 * dt
    S_temp = S + 0.5 * dS_1 * dt
    e_temp = e + 0.5 * de_1 * dt

    dCO2_2 = -2 * CO2_temp**2 * k_f(14) - CO2_temp * CO2_exc_temp * k_f(30) - CO2_temp * O2_temp * k_b(15) - CO2_temp * O2_plus_temp * k_b(18) - CO2_temp * O_temp * k_b(21) - CO2_temp * O_temp * k_f(10) - CO2_temp * e_temp * k_f(1) - CO2_temp * e_temp * k_f(2) - CO2_temp * e_temp * k_f(3) - CO2_temp * e_temp * k_f(4) + CO2_exc_temp * e_temp * k_b(4) + CO2_plus_temp * O2_temp * k_f(18) + CO2_plus_temp * O_minus_temp * k_f(21) + CO2_plus_temp * e_temp**2 * k_b(2) + 2 * CO_temp**2 * O2_temp * k_b(14) + CO_temp**2 * O2_temp * k_b(30) + CO_temp * O2_temp * k_b(10) + CO_temp * O_temp * e_temp * k_b(1) + CO_plus_temp * O_temp * e_temp**2 * k_b(3) + O3_temp * O_temp * k_f(15)
    dCO_2 = 2 * C2_temp * O2_temp * k_f(28) + C2_temp * O_temp * k_b(27) + 2 * CO2_temp**2 * k_f(14) + 2 * CO2_temp * CO2_exc_temp * k_f(30) + CO2_temp * CO2_plus_temp * k_f(17) - CO2_temp * CO_temp * O_plus_temp * k_b(17) + CO2_temp * O_temp * k_f(10) + CO2_temp * e_temp * k_f(1) + CO2_exc_temp * k_f(29) - 2 * CO_temp**2 * O2_temp * k_b(14) - 2 * CO_temp**2 * O2_temp * k_b(30) - 2 * CO_temp**2 * k_b(28) - CO_temp * C_temp * k_f(27) - CO_temp * O2_temp * k_b(10) - CO_temp * O_temp * e_temp * k_b(1) - CO_temp * O_temp * k_b(29) - CO_temp * O_temp * k_f(11) - CO_temp * k_b(25) + C_temp * O2_temp * k_b(11) + C_temp * O_temp * k_f(25)
    dC_2 = C2_temp * O_temp * k_b(27) + 2 * C2_temp * k_b(26) - CO_temp * C_temp * k_f(27) + CO_temp * O_temp * k_f(11) + CO_temp * k_b(25) - 2 * C_temp**2 * k_f(26) - C_temp * O2_temp * k_b(11) - C_temp * O_temp * k_f(25)
    dC2_2 = -C2_temp * O2_temp * k_f(28) - C2_temp * O_temp * k_b(27) - C2_temp * k_b(26) + CO_temp**2 * k_b(28) + CO_temp * C_temp * k_f(27) + C_temp**2 * k_f(26)
    dO_2 = -C2_temp * O_temp * k_b(27) + CO2_temp * O2_temp * k_b(15) - CO2_temp * O_temp * k_b(21) - CO2_temp * O_temp * k_f(10) + CO2_temp * e_temp * k_f(1) + CO2_temp * e_temp * k_f(3) + CO2_exc_temp * k_f(29) + CO2_plus_temp * O_minus_temp * k_f(21) + CO_temp * C_temp * k_f(27) + CO_temp * O2_temp * k_b(10) - CO_temp * O_temp * e_temp * k_b(1) - CO_temp * O_temp * k_b(29) - CO_temp * O_temp * k_f(11) + CO_temp * k_b(25) - CO_plus_temp * O_temp * e_temp**2 * k_b(3) + C_temp * O2_temp * k_b(11) - C_temp * O_temp * k_f(25) - M_temp * O2_temp * O_temp * k_f(13) + 2 * M_temp * O2_temp * k_b(12) + M_temp * O3_temp * k_b(13) - 2 * M_temp * O_temp**2 * k_f(12) - M_temp * O_temp * e_temp * k_f(9) + M_temp * O_minus_temp * k_b(9) + O2_temp**2 * k_b(16) + O2_temp * O2_plus_temp * k_f(19) - O2_temp * O_temp * k_b(20) + O2_temp * e_temp * k_f(7) - O2_minus_temp * O_temp * k_b(7) + O2_minus_temp * O_plus_temp * k_f(20) - O3_temp * O_temp * k_f(15) - O3_temp * O_temp * k_f(16) - O3_plus_temp * O_temp * k_b(19) - O_temp * S_temp * k_f(22) - O_temp * e_temp * k_f(8) + O_ad_temp * k_b(22) + O_minus_temp * e_temp * k_b(8)
    dO2_2 = -C2_temp * O2_temp * k_f(28) + CO2_temp**2 * k_f(14) + CO2_temp * CO2_exc_temp * k_f(30) - CO2_temp * O2_temp * k_b(15) + CO2_temp * O2_plus_temp * k_b(18) + CO2_temp * O_temp * k_f(10) - CO2_plus_temp * O2_temp * k_f(18) - CO_temp**2 * O2_temp * k_b(14) - CO_temp**2 * O2_temp * k_b(30) + CO_temp**2 * k_b(28) - CO_temp * O2_temp * k_b(10) + CO_temp * O_temp * k_f(11) - C_temp * O2_temp * k_b(11) - M_temp * O2_temp * O_temp * k_f(13) - M_temp * O2_temp * k_b(12) + M_temp * O3_temp * k_b(13) + M_temp * O_temp**2 * k_f(12) - 2 * O2_temp**2 * k_b(16) - O2_temp * O2_plus_temp * k_f(19) - O2_temp * O_temp * k_b(20) - O2_temp * O_ad_temp * k_b(24) - O2_temp * e_temp * k_f(5) - O2_temp * e_temp * k_f(6) - O2_temp * e_temp * k_f(7) - O2_temp * k_b(23) + O2_exc_temp * e_temp * k_b(5) + O2_minus_temp * O_temp * k_b(7) + O2_minus_temp * O_plus_temp * k_f(20) + O2_plus_temp * e_temp**2 * k_b(6) + O3_temp * O_temp * k_f(15) + 2 * O3_temp * O_temp * k_f(16) + O3_temp * S_temp * k_f(24) + O3_plus_temp * O_temp * k_b(19) + O_ad_temp**2 * k_f(23)
    dO3_2 = CO2_temp * O2_temp * k_b(15) + M_temp * O2_temp * O_temp * k_f(13) - M_temp * O3_temp * k_b(13) + O2_temp**2 * k_b(16) + O2_temp * O_ad_temp * k_b(24) - O3_temp * O_temp * k_f(15) - O3_temp * O_temp * k_f(16) - O3_temp * S_temp * k_f(24)
    dCO2_plus_2 = -CO2_temp * CO2_plus_temp * k_f(17) + CO2_temp * CO_temp * O_plus_temp * k_b(17) + CO2_temp * O2_plus_temp * k_b(18) + CO2_temp * O_temp * k_b(21) + CO2_temp * e_temp * k_f(2) - CO2_plus_temp * O2_temp * k_f(18) - CO2_plus_temp * O_minus_temp * k_f(21) - CO2_plus_temp * e_temp**2 * k_b(2)
    dCO_plus_2 = CO2_temp * e_temp * k_f(3) - CO_plus_temp * O_temp * e_temp**2 * k_b(3)
    dO3_plus_2 = O2_temp * O2_plus_temp * k_f(19) - O3_plus_temp * O_temp * k_b(19)
    dO2_plus_2 = -CO2_temp * O2_plus_temp * k_b(18) + CO2_plus_temp * O2_temp * k_f(18) - O2_temp * O2_plus_temp * k_f(19) + O2_temp * e_temp * k_f(6) - O2_plus_temp * e_temp**2 * k_b(6) + O3_plus_temp * O_temp * k_b(19)
    dO_plus_2 = CO2_temp * CO2_plus_temp * k_f(17) - CO2_temp * CO_temp * O_plus_temp * k_b(17) + O2_temp * O_temp * k_b(20) - O2_minus_temp * O_plus_temp * k_f(20)
    dO2_minus_2 = O2_temp * O_temp * k_b(20) - O2_minus_temp * O_plus_temp * k_f(20) + O2_temp * e_temp * k_f(7) + O2_temp * e_temp * k_f(5)
    dO_minus_2 = CO2_temp * O_temp * k_b(21) - CO2_plus_temp * O_minus_temp * k_f(21) + M_temp * O_temp * e_temp * k_f(9) - M_temp * O_minus_temp * k_b(9) + O_temp * e_temp * k_f(8) - O_minus_temp * e_temp * k_b(8)
    dCO2_exc_2 = -CO2_temp * CO2_exc_temp * k_f(30) + CO2_temp * e_temp * k_f(4) - CO2_exc_temp * e_temp * k_b(4) - CO2_exc_temp * k_f(29) + CO_temp**2 * O2_temp * k_b(30) + CO_temp * O_temp * k_b(29)
    dO2_exc_2 = O2_temp * e_temp * k_f(5) - O2_exc_temp * e_temp * k_b(5)
    dO_ad_2 = -O2_temp * O_ad_temp * k_b(24) + 2 * O2_temp * k_b(23) + O3_temp * S_temp * k_f(24) + O_temp * S_temp * k_f(22) - 2 * O_ad_temp**2 * k_f(23) - O_ad_temp * k_b(22)
    dM_2 = 0
    dS_2 = O2_temp * O_ad_temp * k_b(24) - O3_temp * S_temp * k_f(24) - O_temp * S_temp * k_f(22) + O_ad_temp * k_b(22)
    de_2 = CO2_temp * e_temp * k_f(2) + CO2_temp * e_temp * k_f(3) - CO2_plus_temp * e_temp**2 * k_b(2) - CO_plus_temp * O_temp * e_temp**2 * k_b(3) - M_temp * O_temp * e_temp * k_f(9) + M_temp * O_minus_temp * k_b(9) + O2_temp * e_temp * k_f(6) - O2_temp * e_temp * k_f(7) + O2_minus_temp * O_temp * k_b(7) - O2_plus_temp * e_temp**2 * k_b(6)

    CO2_temp = CO2 + 0.5 * dCO2_2 * dt
    CO_temp = CO + 0.5 * dCO_2 * dt
    C_temp = C + 0.5 * dC_2 * dt
    C2_temp = C2 + 0.5 * dC2_2 * dt
    O_temp = O + 0.5 * dO_2 * dt
    O2_temp = O2 + 0.5 * dO2_2 * dt
    O3_temp = O3 + 0.5 * dO3_2 * dt
    CO2_plus_temp = CO2_plus + 0.5 * dCO2_plus_2 * dt
    CO_plus_temp = CO_plus + 0.5 * dCO_plus_2 * dt
    O3_plus_temp = O3_plus + 0.5 * dO3_plus_2 * dt
    O2_plus_temp = O2_plus + 0.5 * dO2_plus_2 * dt
    O_plus_temp = O_plus + 0.5 * dO_plus_2 * dt
    O2_minus_temp = O2_minus + 0.5 * dO2_minus_2 * dt
    O_minus_temp = O_minus + 0.5 * dO_minus_2 * dt
    CO2_exc_temp = CO2_exc + 0.5 * dCO2_exc_2 * dt
    O2_exc_temp = O2_exc + 0.5 * dO2_exc_2 * dt
    O_ad_temp = O_ad + 0.5 * dO_ad_2 * dt
    M_temp = M + 0.5 * dM_2 * dt
    S_temp = S + 0.5 * dS_2 * dt
    e_temp = e + 0.5 * de_2 * dt
    
    dCO2_3 = -2 * CO2_temp**2 * k_f(14) - CO2_temp * CO2_exc_temp * k_f(30) - CO2_temp * O2_temp * k_b(15) - CO2_temp * O2_plus_temp * k_b(18) - CO2_temp * O_temp * k_b(21) - CO2_temp * O_temp * k_f(10) - CO2_temp * e_temp * k_f(1) - CO2_temp * e_temp * k_f(2) - CO2_temp * e_temp * k_f(3) - CO2_temp * e_temp * k_f(4) + CO2_exc_temp * e_temp * k_b(4) + CO2_plus_temp * O2_temp * k_f(18) + CO2_plus_temp * O_minus_temp * k_f(21) + CO2_plus_temp * e_temp**2 * k_b(2) + 2 * CO_temp**2 * O2_temp * k_b(14) + CO_temp**2 * O2_temp * k_b(30) + CO_temp * O2_temp * k_b(10) + CO_temp * O_temp * e_temp * k_b(1) + CO_plus_temp * O_temp * e_temp**2 * k_b(3) + O3_temp * O_temp * k_f(15)
    dCO_3 = 2 * C2_temp * O2_temp * k_f(28) + C2_temp * O_temp * k_b(27) + 2 * CO2_temp**2 * k_f(14) + 2 * CO2_temp * CO2_exc_temp * k_f(30) + CO2_temp * CO2_plus_temp * k_f(17) - CO2_temp * CO_temp * O_plus_temp * k_b(17) + CO2_temp * O_temp * k_f(10) + CO2_temp * e_temp * k_f(1) + CO2_exc_temp * k_f(29) - 2 * CO_temp**2 * O2_temp * k_b(14) - 2 * CO_temp**2 * O2_temp * k_b(30) - 2 * CO_temp**2 * k_b(28) - CO_temp * C_temp * k_f(27) - CO_temp * O2_temp * k_b(10) - CO_temp * O_temp * e_temp * k_b(1) - CO_temp * O_temp * k_b(29) - CO_temp * O_temp * k_f(11) - CO_temp * k_b(25) + C_temp * O2_temp * k_b(11) + C_temp * O_temp * k_f(25)
    dC_3 = C2_temp * O_temp * k_b(27) + 2 * C2_temp * k_b(26) - CO_temp * C_temp * k_f(27) + CO_temp * O_temp * k_f(11) + CO_temp * k_b(25) - 2 * C_temp**2 * k_f(26) - C_temp * O2_temp * k_b(11) - C_temp * O_temp * k_f(25)
    dC2_3 = -C2_temp * O2_temp * k_f(28) - C2_temp * O_temp * k_b(27) - C2_temp * k_b(26) + CO_temp**2 * k_b(28) + CO_temp * C_temp * k_f(27) + C_temp**2 * k_f(26)
    dO_3 = -C2_temp * O_temp * k_b(27) + CO2_temp * O2_temp * k_b(15) - CO2_temp * O_temp * k_b(21) - CO2_temp * O_temp * k_f(10) + CO2_temp * e_temp * k_f(1) + CO2_temp * e_temp * k_f(3) + CO2_exc_temp * k_f(29) + CO2_plus_temp * O_minus_temp * k_f(21) + CO_temp * C_temp * k_f(27) + CO_temp * O2_temp * k_b(10) - CO_temp * O_temp * e_temp * k_b(1) - CO_temp * O_temp * k_b(29) - CO_temp * O_temp * k_f(11) + CO_temp * k_b(25) - CO_plus_temp * O_temp * e_temp**2 * k_b(3) + C_temp * O2_temp * k_b(11) - C_temp * O_temp * k_f(25) - M_temp * O2_temp * O_temp * k_f(13) + 2 * M_temp * O2_temp * k_b(12) + M_temp * O3_temp * k_b(13) - 2 * M_temp * O_temp**2 * k_f(12) - M_temp * O_temp * e_temp * k_f(9) + M_temp * O_minus_temp * k_b(9) + O2_temp**2 * k_b(16) + O2_temp * O2_plus_temp * k_f(19) - O2_temp * O_temp * k_b(20) + O2_temp * e_temp * k_f(7) - O2_minus_temp * O_temp * k_b(7) + O2_minus_temp * O_plus_temp * k_f(20) - O3_temp * O_temp * k_f(15) - O3_temp * O_temp * k_f(16) - O3_plus_temp * O_temp * k_b(19) - O_temp * S_temp * k_f(22) - O_temp * e_temp * k_f(8) + O_ad_temp * k_b(22) + O_minus_temp * e_temp * k_b(8)
    dO2_3 = -C2_temp * O2_temp * k_f(28) + CO2_temp**2 * k_f(14) + CO2_temp * CO2_exc_temp * k_f(30) - CO2_temp * O2_temp * k_b(15) + CO2_temp * O2_plus_temp * k_b(18) + CO2_temp * O_temp * k_f(10) - CO2_plus_temp * O2_temp * k_f(18) - CO_temp**2 * O2_temp * k_b(14) - CO_temp**2 * O2_temp * k_b(30) + CO_temp**2 * k_b(28) - CO_temp * O2_temp * k_b(10) + CO_temp * O_temp * k_f(11) - C_temp * O2_temp * k_b(11) - M_temp * O2_temp * O_temp * k_f(13) - M_temp * O2_temp * k_b(12) + M_temp * O3_temp * k_b(13) + M_temp * O_temp**2 * k_f(12) - 2 * O2_temp**2 * k_b(16) - O2_temp * O2_plus_temp * k_f(19) - O2_temp * O_temp * k_b(20) - O2_temp * O_ad_temp * k_b(24) - O2_temp * e_temp * k_f(5) - O2_temp * e_temp * k_f(6) - O2_temp * e_temp * k_f(7) - O2_temp * k_b(23) + O2_exc_temp * e_temp * k_b(5) + O2_minus_temp * O_temp * k_b(7) + O2_minus_temp * O_plus_temp * k_f(20) + O2_plus_temp * e_temp**2 * k_b(6) + O3_temp * O_temp * k_f(15) + 2 * O3_temp * O_temp * k_f(16) + O3_temp * S_temp * k_f(24) + O3_plus_temp * O_temp * k_b(19) + O_ad_temp**2 * k_f(23)
    dO3_3 = CO2_temp * O2_temp * k_b(15) + M_temp * O2_temp * O_temp * k_f(13) - M_temp * O3_temp * k_b(13) + O2_temp**2 * k_b(16) + O2_temp * O_ad_temp * k_b(24) - O3_temp * O_temp * k_f(15) - O3_temp * O_temp * k_f(16) - O3_temp * S_temp * k_f(24)
    dCO2_plus_3 = -CO2_temp * CO2_plus_temp * k_f(17) + CO2_temp * CO_temp * O_plus_temp * k_b(17) + CO2_temp * O2_plus_temp * k_b(18) + CO2_temp * O_temp * k_b(21) + CO2_temp * e_temp * k_f(2) - CO2_plus_temp * O2_temp * k_f(18) - CO2_plus_temp * O_minus_temp * k_f(21) - CO2_plus_temp * e_temp**2 * k_b(2)
    dCO_plus_3 = CO2_temp * e_temp * k_f(3) - CO_plus_temp * O_temp * e_temp**2 * k_b(3)
    dO3_plus_3 = O2_temp * O2_plus_temp * k_f(19) - O3_plus_temp * O_temp * k_b(19)
    dO2_plus_3 = -CO2_temp * O2_plus_temp * k_b(18) + CO2_plus_temp * O2_temp * k_f(18) - O2_temp * O2_plus_temp * k_f(19) + O2_temp * e_temp * k_f(6) - O2_plus_temp * e_temp**2 * k_b(6) + O3_plus_temp * O_temp * k_b(19)
    dO_plus_3 = CO2_temp * CO2_plus_temp * k_f(17) - CO2_temp * CO_temp * O_plus_temp * k_b(17) + O2_temp * O_temp * k_b(20) - O2_minus_temp * O_plus_temp * k_f(20)
    dO2_minus_3 = O2_temp * O_temp * k_b(20) - O2_minus_temp * O_plus_temp * k_f(20) + O2_temp * e_temp * k_f(7) + O2_temp * e_temp * k_f(5)
    dO_minus_3 = CO2_temp * O_temp * k_b(21) - CO2_plus_temp * O_minus_temp * k_f(21) + M_temp * O_temp * e_temp * k_f(9) - M_temp * O_minus_temp * k_b(9) + O_temp * e_temp * k_f(8) - O_minus_temp * e_temp * k_b(8)
    dCO2_exc_3 = -CO2_temp * CO2_exc_temp * k_f(30) + CO2_temp * e_temp * k_f(4) - CO2_exc_temp * e_temp * k_b(4) - CO2_exc_temp * k_f(29) + CO_temp**2 * O2_temp * k_b(30) + CO_temp * O_temp * k_b(29)
    dO2_exc_3 = O2_temp * e_temp * k_f(5) - O2_exc_temp * e_temp * k_b(5)
    dO_ad_3 = -O2_temp * O_ad_temp * k_b(24) + 2 * O2_temp * k_b(23) + O3_temp * S_temp * k_f(24) + O_temp * S_temp * k_f(22) - 2 * O_ad_temp**2 * k_f(23) - O_ad_temp * k_b(22)
    dM_3 = 0
    dS_3 = O2_temp * O_ad_temp * k_b(24) - O3_temp * S_temp * k_f(24) - O_temp * S_temp * k_f(22) + O_ad_temp * k_b(22)
    de_3 = CO2_temp * e_temp * k_f(2) + CO2_temp * e_temp * k_f(3) - CO2_plus_temp * e_temp**2 * k_b(2) - CO_plus_temp * O_temp * e_temp**2 * k_b(3) - M_temp * O_temp * e_temp * k_f(9) + M_temp * O_minus_temp * k_b(9) + O2_temp * e_temp * k_f(6) - O2_temp * e_temp * k_f(7) + O2_minus_temp * O_temp * k_b(7) - O2_plus_temp * e_temp**2 * k_b(6)

    CO2_temp = CO2 + 0.5 * dCO2_3 * dt
    CO_temp = CO + 0.5 * dCO_3 * dt
    C_temp = C + 0.5 * dC_3 * dt
    C2_temp = C2 + 0.5 * dC2_3 * dt
    O_temp = O + 0.5 * dO_3 * dt
    O2_temp = O2 + 0.5 * dO2_3 * dt
    O3_temp = O3 + 0.5 * dO3_3 * dt
    CO2_plus_temp = CO2_plus + 0.5 * dCO2_plus_3 * dt
    CO_plus_temp = CO_plus + 0.5 * dCO_plus_3 * dt
    O3_plus_temp = O3_plus + 0.5 * dO3_plus_3 * dt
    O2_plus_temp = O2_plus + 0.5 * dO2_plus_3 * dt
    O_plus_temp = O_plus + 0.5 * dO_plus_3 * dt
    O2_minus_temp = O2_minus + 0.5 * dO2_minus_3 * dt
    O_minus_temp = O_minus + 0.5 * dO_minus_3 * dt
    CO2_exc_temp = CO2_exc + 0.5 * dCO2_exc_3 * dt
    O2_exc_temp = O2_exc + 0.5 * dO2_exc_3 * dt
    O_ad_temp = O_ad + 0.5 * dO_ad_3 * dt
    M_temp = M + 0.5 * dM_3 * dt
    S_temp = S + 0.5 * dS_3 * dt
    e_temp = e + 0.5 * de_3 * dt

    dCO2_4 = -2 * CO2_temp**2 * k_f(14) - CO2_temp * CO2_exc_temp * k_f(30) - CO2_temp * O2_temp * k_b(15) - CO2_temp * O2_plus_temp * k_b(18) - CO2_temp * O_temp * k_b(21) - CO2_temp * O_temp * k_f(10) - CO2_temp * e_temp * k_f(1) - CO2_temp * e_temp * k_f(2) - CO2_temp * e_temp * k_f(3) - CO2_temp * e_temp * k_f(4) + CO2_exc_temp * e_temp * k_b(4) + CO2_plus_temp * O2_temp * k_f(18) + CO2_plus_temp * O_minus_temp * k_f(21) + CO2_plus_temp * e_temp**2 * k_b(2) + 2 * CO_temp**2 * O2_temp * k_b(14) + CO_temp**2 * O2_temp * k_b(30) + CO_temp * O2_temp * k_b(10) + CO_temp * O_temp * e_temp * k_b(1) + CO_plus_temp * O_temp * e_temp**2 * k_b(3) + O3_temp * O_temp * k_f(15)
    dCO_4 = 2 * C2_temp * O2_temp * k_f(28) + C2_temp * O_temp * k_b(27) + 2 * CO2_temp**2 * k_f(14) + 2 * CO2_temp * CO2_exc_temp * k_f(30) + CO2_temp * CO2_plus_temp * k_f(17) - CO2_temp * CO_temp * O_plus_temp * k_b(17) + CO2_temp * O_temp * k_f(10) + CO2_temp * e_temp * k_f(1) + CO2_exc_temp * k_f(29) - 2 * CO_temp**2 * O2_temp * k_b(14) - 2 * CO_temp**2 * O2_temp * k_b(30) - 2 * CO_temp**2 * k_b(28) - CO_temp * C_temp * k_f(27) - CO_temp * O2_temp * k_b(10) - CO_temp * O_temp * e_temp * k_b(1) - CO_temp * O_temp * k_b(29) - CO_temp * O_temp * k_f(11) - CO_temp * k_b(25) + C_temp * O2_temp * k_b(11) + C_temp * O_temp * k_f(25)
    dC_4 = C2_temp * O_temp * k_b(27) + 2 * C2_temp * k_b(26) - CO_temp * C_temp * k_f(27) + CO_temp * O_temp * k_f(11) + CO_temp * k_b(25) - 2 * C_temp**2 * k_f(26) - C_temp * O2_temp * k_b(11) - C_temp * O_temp * k_f(25)
    dC2_4 = -C2_temp * O2_temp * k_f(28) - C2_temp * O_temp * k_b(27) - C2_temp * k_b(26) + CO_temp**2 * k_b(28) + CO_temp * C_temp * k_f(27) + C_temp**2 * k_f(26)
    dO_4 = -C2_temp * O_temp * k_b(27) + CO2_temp * O2_temp * k_b(15) - CO2_temp * O_temp * k_b(21) - CO2_temp * O_temp * k_f(10) + CO2_temp * e_temp * k_f(1) + CO2_temp * e_temp * k_f(3) + CO2_exc_temp * k_f(29) + CO2_plus_temp * O_minus_temp * k_f(21) + CO_temp * C_temp * k_f(27) + CO_temp * O2_temp * k_b(10) - CO_temp * O_temp * e_temp * k_b(1) - CO_temp * O_temp * k_b(29) - CO_temp * O_temp * k_f(11) + CO_temp * k_b(25) - CO_plus_temp * O_temp * e_temp**2 * k_b(3) + C_temp * O2_temp * k_b(11) - C_temp * O_temp * k_f(25) - M_temp * O2_temp * O_temp * k_f(13) + 2 * M_temp * O2_temp * k_b(12) + M_temp * O3_temp * k_b(13) - 2 * M_temp * O_temp**2 * k_f(12) - M_temp * O_temp * e_temp * k_f(9) + M_temp * O_minus_temp * k_b(9) + O2_temp**2 * k_b(16) + O2_temp * O2_plus_temp * k_f(19) - O2_temp * O_temp * k_b(20) + O2_temp * e_temp * k_f(7) - O2_minus_temp * O_temp * k_b(7) + O2_minus_temp * O_plus_temp * k_f(20) - O3_temp * O_temp * k_f(15) - O3_temp * O_temp * k_f(16) - O3_plus_temp * O_temp * k_b(19) - O_temp * S_temp * k_f(22) - O_temp * e_temp * k_f(8) + O_ad_temp * k_b(22) + O_minus_temp * e_temp * k_b(8)
    dO2_4 = -C2_temp * O2_temp * k_f(28) + CO2_temp**2 * k_f(14) + CO2_temp * CO2_exc_temp * k_f(30) - CO2_temp * O2_temp * k_b(15) + CO2_temp * O2_plus_temp * k_b(18) + CO2_temp * O_temp * k_f(10) - CO2_plus_temp * O2_temp * k_f(18) - CO_temp**2 * O2_temp * k_b(14) - CO_temp**2 * O2_temp * k_b(30) + CO_temp**2 * k_b(28) - CO_temp * O2_temp * k_b(10) + CO_temp * O_temp * k_f(11) - C_temp * O2_temp * k_b(11) - M_temp * O2_temp * O_temp * k_f(13) - M_temp * O2_temp * k_b(12) + M_temp * O3_temp * k_b(13) + M_temp * O_temp**2 * k_f(12) - 2 * O2_temp**2 * k_b(16) - O2_temp * O2_plus_temp * k_f(19) - O2_temp * O_temp * k_b(20) - O2_temp * O_ad_temp * k_b(24) - O2_temp * e_temp * k_f(5) - O2_temp * e_temp * k_f(6) - O2_temp * e_temp * k_f(7) - O2_temp * k_b(23) + O2_exc_temp * e_temp * k_b(5) + O2_minus_temp * O_temp * k_b(7) + O2_minus_temp * O_plus_temp * k_f(20) + O2_plus_temp * e_temp**2 * k_b(6) + O3_temp * O_temp * k_f(15) + 2 * O3_temp * O_temp * k_f(16) + O3_temp * S_temp * k_f(24) + O3_plus_temp * O_temp * k_b(19) + O_ad_temp**2 * k_f(23)
    dO3_4 = CO2_temp * O2_temp * k_b(15) + M_temp * O2_temp * O_temp * k_f(13) - M_temp * O3_temp * k_b(13) + O2_temp**2 * k_b(16) + O2_temp * O_ad_temp * k_b(24) - O3_temp * O_temp * k_f(15) - O3_temp * O_temp * k_f(16) - O3_temp * S_temp * k_f(24)
    dCO2_plus_4 = -CO2_temp * CO2_plus_temp * k_f(17) + CO2_temp * CO_temp * O_plus_temp * k_b(17) + CO2_temp * O2_plus_temp * k_b(18) + CO2_temp * O_temp * k_b(21) + CO2_temp * e_temp * k_f(2) - CO2_plus_temp * O2_temp * k_f(18) - CO2_plus_temp * O_minus_temp * k_f(21) - CO2_plus_temp * e_temp**2 * k_b(2)
    dCO_plus_4 = CO2_temp * e_temp * k_f(3) - CO_plus_temp * O_temp * e_temp**2 * k_b(3)
    dO3_plus_4 = O2_temp * O2_plus_temp * k_f(19) - O3_plus_temp * O_temp * k_b(19)
    dO2_plus_4 = -CO2_temp * O2_plus_temp * k_b(18) + CO2_plus_temp * O2_temp * k_f(18) - O2_temp * O2_plus_temp * k_f(19) + O2_temp * e_temp * k_f(6) - O2_plus_temp * e_temp**2 * k_b(6) + O3_plus_temp * O_temp * k_b(19)
    dO_plus_4 = CO2_temp * CO2_plus_temp * k_f(17) - CO2_temp * CO_temp * O_plus_temp * k_b(17) + O2_temp * O_temp * k_b(20) - O2_minus_temp * O_plus_temp * k_f(20)
    dO2_minus_4 = O2_temp * O_temp * k_b(20) - O2_minus_temp * O_plus_temp * k_f(20) + O2_temp * e_temp * k_f(7) + O2_temp * e_temp * k_f(5)
    dO_minus_4 = CO2_temp * O_temp * k_b(21) - CO2_plus_temp * O_minus_temp * k_f(21) + M_temp * O_temp * e_temp * k_f(9) - M_temp * O_minus_temp * k_b(9) + O_temp * e_temp * k_f(8) - O_minus_temp * e_temp * k_b(8)
    dCO2_exc_4 = -CO2_temp * CO2_exc_temp * k_f(30) + CO2_temp * e_temp * k_f(4) - CO2_exc_temp * e_temp * k_b(4) - CO2_exc_temp * k_f(29) + CO_temp**2 * O2_temp * k_b(30) + CO_temp * O_temp * k_b(29)
    dO2_exc_4 = O2_temp * e_temp * k_f(5) - O2_exc_temp * e_temp * k_b(5)
    dO_ad_4 = -O2_temp * O_ad_temp * k_b(24) + 2 * O2_temp * k_b(23) + O3_temp * S_temp * k_f(24) + O_temp * S_temp * k_f(22) - 2 * O_ad_temp**2 * k_f(23) - O_ad_temp * k_b(22)
    dM_4 = 0
    dS_4 = O2_temp * O_ad_temp * k_b(24) - O3_temp * S_temp * k_f(24) - O_temp * S_temp * k_f(22) + O_ad_temp * k_b(22)
    de_4 = CO2_temp * e_temp * k_f(2) + CO2_temp * e_temp * k_f(3) - CO2_plus_temp * e_temp**2 * k_b(2) - CO_plus_temp * O_temp * e_temp**2 * k_b(3) - M_temp * O_temp * e_temp * k_f(9) + M_temp * O_minus_temp * k_b(9) + O2_temp * e_temp * k_f(6) - O2_temp * e_temp * k_f(7) + O2_minus_temp * O_temp * k_b(7) - O2_plus_temp * e_temp**2 * k_b(6)

    CO2 = CO2 + (dt / 6.0) * (dCO2_1 + 2 * dCO2_2 + 2 * dCO2_3 + dCO2_4)
    CO = CO + (dt / 6.0) * (dCO_1 + 2 * dCO_2 + 2 * dCO_3 + dCO_4)
    C = C + (dt / 6.0) * (dC_1 + 2 * dC_2 + 2 * dC_3 + dC_4)
    C2 = C2 + (dt / 6.0) * (dC2_1 + 2 * dC2_2 + 2 * dC2_3 + dC2_4)
    O = O + (dt / 6.0) * (dO_1 + 2 * dO_2 + 2 * dO_3 + dO_4)
    O2 = O2 + (dt / 6.0) * (dO2_1 + 2 * dO2_2 + 2 * dO2_3 + dO2_4)
    O3 = O3 + (dt / 6.0) * (dO3_1 + 2 * dO3_2 + 2 * dO3_3 + dO3_4)
    CO2_plus = CO2_plus + (dt / 6.0) * (dCO2_plus_1 + 2 * dCO2_plus_2 + 2 * dCO2_plus_3 + dCO2_plus_4)
    CO_plus = CO_plus + (dt / 6.0) * (dCO_plus_1 + 2 * dCO_plus_2 + 2 * dCO_plus_3 + dCO_plus_4)
    O3_plus = O3_plus + (dt / 6.0) * (dO3_plus_1 + 2 * dO3_plus_2 + 2 * dO3_plus_3 + dO3_plus_4)
    O2_plus = O2_plus + (dt / 6.0) * (dO2_plus_1 + 2 * dO2_plus_2 + 2 * dO2_plus_3 + dO2_plus_4)
    O_plus = O_plus + (dt / 6.0) * (dO_plus_1 + 2 * dO_plus_2 + 2 * dO_plus_3 + dO_plus_4)
    O2_minus = O2_minus + (dt / 6.0) * (dO2_minus_1 + 2 * dO2_minus_2 + 2 * dO2_minus_3 + dO2_minus_4)
    O_minus = O_minus + (dt / 6.0) * (dO_minus_1 + 2 * dO_minus_2 + 2 * dO_minus_3 + dO_minus_4)
    CO2_exc = CO2_exc + (dt / 6.0) * (dCO2_exc_1 + 2 * dCO2_exc_2 + 2 * dCO2_exc_3 + dCO2_exc_4)
    O2_exc = O2_exc + (dt / 6.0) * (dO2_exc_1 + 2 * dO2_exc_2 + 2 * dO2_exc_3 + dO2_exc_4)
    O_ad = O_ad + (dt / 6.0) * (dO_ad_1 + 2 * dO_ad_2 + 2 * dO_ad_3 + dO_ad_4)
    M = M + (dt / 6.0) * (dM_1 + 2 * dM_2 + 2 * dM_3 + dM_4)
    S = S + (dt / 6.0) * (dS_1 + 2 * dS_2 + 2 * dS_3 + dS_4)
    e = e + (dt / 6.0) * (de_1 + 2 * de_2 + 2 * de_3 + de_4)
    
    CO2 = CO2
    CO = CO * 0.5
    C = C
    C2 = C2
    O = O * 1E-51_8
    O2 = O2 * (-1.25E-3)
    O3 = O3 
    CO2_plus = CO2_plus * (-1)
    CO_plus = CO_plus * 1E16
    O3_plus = O3_plus * (-1E36_8)
    O2_plus = O2_plus * 1E14
    O_plus = O_plus * 1E-9
    O2_minus = O2_minus * 1E-29
    O_minus = O_minus * 1E-9
    CO2_exc = CO2_exc * 1E15
    O2_exc = O2_exc * 1E38
    O_ad = O_ad
    M = M
    S = S
    e = e

    if (t > t_discharge) then
        if ((CO2 /= CO2) .or. CO2 < 0.0 .or. CO2 > n_CO2) then
            CO2 = CO2_old
        end if

        if ((CO /= CO) .or. CO < 0.0 .or. CO > n_CO2) then
            CO = CO_old
        end if

        if ((C /= C) .or. C < 0.0 .or. C > n_CO2) then
            C = C_old
        end if

        if ((C2 /= C2) .or. C2 < 0.0 .or. C2 > n_CO2) then
            C2 = C2_old
        end if

        if ((O /= O) .or. O < 0.0 .or. O > n_CO2) then
            O = O_old
        end if

        if ((O2 /= O2) .or. O2 < 0.0 .or. O2 > n_CO2) then
            O2 = O2_old
        end if

        if ((O3 /= O3) .or. O3 < 0.0 .or. O3 > n_CO2) then
            O3 = O3_old
        end if

        if ((CO2_plus /= CO2_plus) .or. CO2_plus < 0.0 .or. CO2_plus > n_CO2) then
            CO2_plus = CO2_plus_old
        end if

        if ((CO_plus /= CO_plus) .or. CO_plus < 0.0 .or. CO_plus > n_CO2) then
            CO_plus = CO_plus_old
        end if 

        if ((O3_plus /= O3_plus) .or. O3_plus < 0.0 .or. O3_plus > n_CO2) then
            O3_plus = O3_plus_old
        end if

        if ((O2_plus /= O2_plus) .or. O2_plus < 0.0 .or. O2_plus > n_CO2) then
            O2_plus = O2_plus_old
        end if

        if ((O_plus /= O_plus) .or. O_plus < 0.0 .or. O_plus > n_CO2) then
            O_plus = O_plus_old
        end if 

        if ((O2_minus /= O2_minus) .or. O2_minus < 0.0 .or. O2_minus > n_CO2) then
            O2_minus = O2_minus_old
        end if

        if ((O_minus /= O_minus) .or. O_minus < 0.0 .or. O_minus > n_CO2) then
            O_minus = O_minus_old
        end if

        if ((CO2_exc /= CO2_exc) .or. CO2_exc < 0.0 .or. CO2_exc > n_CO2) then
            CO2_exc = CO2_exc_old
        end if

        if ((O2_exc /= O2_exc) .or. O2_exc < 0.0 .or. O2_exc > n_CO2) then
            O2_exc = O2_exc_old
        end if

        if ((O_ad /= O_ad) .or. O_ad < 0.0 .or. O_ad > n_CO2) then
            O_ad = O_ad_old
        end if

        if ((M /= M) .or. M < 0.0 .or. M > n_CO2) then
            M = M_old
        end if

        if ((S /= S) .or. S < 0.0 .or. S > n_CO2) then
            S = S_old
        end if

        if ((e /= e) .or. e < 0.0 .or. e > n_CO2) then
            e = e_old
        end if
    end if
end subroutine kinetics_step