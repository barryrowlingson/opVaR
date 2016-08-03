key.sum <-
function (v, u) 
{
    wym <- {
    }
    j = 1
    k = 1
    for (i in 1:length(u)) {
        if (u[i] != 0) {
            wym[k] = sum(v[j:(j + u[i] - 1)])
            k = k + 1
        }
        j <- j + u[i]
    }
    if (length(v) != sum(u)) {
        print("There is not enough u or v data")
    }
    wym
}
