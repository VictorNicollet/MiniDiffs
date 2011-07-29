var MiniDiff = {

    apply : function(diff,source) {
	
	var out  = [], 
            data = diff[0], 
            read = 0, 
            ops  = diff.slice(1),
            size = 0;
	
	for (var i = 0; i < ops.length; ++i) {
	    var op = ops[i];
	    if (typeof op == "number") {
		out.push(data.substring(read,read+op));
		read += op;
		size += op;
	    } else {
		out.push(source.substring(op[0]+size,op[0]+size+op[1]));
		size += op[1]
	    }
	}
	
	return out.join('');
    }
}
