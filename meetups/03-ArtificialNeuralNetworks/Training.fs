module Training

open ANN

//type TrainResult = {
//    input : float list
//    target : float list
//    output : float list
//    newNet : float list list list
//}
//
//let trainOnce net input target =
//    let newNet = backPropagate net input target
//    {   input = input
//        target = target
//        output = output
//        newNet = newNet 
//    }

let calcVectorLengthSquared target output =
    (0., target, output) |||> List.fold2 (fun s t o -> s + (t - o)**2.)


     
