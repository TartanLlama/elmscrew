var node = document.getElementById('main');
var app = Elm.Main.embed(node);

var network;

app.ports.sendCurrentNode.subscribe(function(nodeId) {
    network.selectNodes(network.findNode(nodeId));
});

app.ports.displayGraph.subscribe(function(data) {
    var nodes = new vis.DataSet(data[0]);
    var edges = new vis.DataSet(data[1]);
    var container = document.getElementById('viewport');

    var data = { nodes: nodes, edges: edges };
    var options = {
        width: '700px', height: '500px',
        edges: { arrows: { to: {enabled: true} }, length: 1, color: { color: '#606c76' } },
        nodes: { color: { background: '#9b4dca', border: '#9b4dca', highlight: '#e0abff' },
                 font: { color: '#fff', face: 'Roboto', size: 16 } }
    };

    network = new vis.Network(container, data, options);
});
