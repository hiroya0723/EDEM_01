program main
    use MethodModule
    use DEMModule
    
    implicit none

    real(8), allocatable :: points(:,:)
    real(8), allocatable :: springs(:,:,:)
    real(8), allocatable :: u(:,:), d(:,:), d0(:,:)
    real(8), allocatable :: x_new(:,:), x(:,:), x_old(:,:)
    real(8), allocatable :: x_condition(:,:)
    real(8), allocatable :: e_mat(:,:,:), F_all(:,:,:), F(:,:)
    integer, allocatable :: spring_flag(:,:)
    
    integer n, m
    
    points = readFile('point.dat', 9)
    n = size(points, 1) !要素数

    springs = makeSpring('spring.dat', n)
    d0 = initD(points)

    x_new = initX(points)
    F = initF('condition.dat')
    x_condition = initXContion('condition.dat')
    allocate(x(n,3))
    allocate(u(n,3))
    allocate(x_old(n,3))
    allocate(d(n,n))
    allocate(spring_flag(n,n))
    allocate(e_mat(n,n,4))
    allocate(F_all(3,n,n))
    
    x = 0.0d0
    x_old = 0.0d0
    u = 0.0d0
    d = 0.0d0
    e_mat = 0.0d0
    spring_flag = 0
    do m = 1,2
        print *, "loop"
        x_old = x
        x = x_new
        call updateF_all(x, u, points(:,4), e_mat, d, d0, spring_flag, springs, F_all)
        call makeF(F_all, F)
        call solveEquation(F, points(:,5:), x_new, x, x_old, x_condition)
        u = x_new - x
    enddo
    call printArray2(x_new)
    !call printArray3(e_mat)
    !call printArray2(F)
end program main