
var classData = {
    // Dates
    from: new Date('2016-09-01'),
    to: new Date('2016-10-30'),

    // Alunos
    students: [
        {id:  0, name: 'Pele Silva'},
        {id:  1, name: 'Romário Martins'},
        {id:  2, name: 'Ronaldo Farias'},
        {id:  3, name: 'Zico Moraes'},
        {id:  4, name: 'Rivelino Gonçalves'},
        {id:  5, name: 'Guido Peres'},
        {id:  6, name: 'Garrincha Soares'},
        {id:  7, name: 'Roberto Dinamite'},
        {id:  8, name: 'Zinedine Zidane'},
        {id:  9, name: 'Raul Castro'},
        {id: 10, name: 'Lionel Messi'},
        {id: 11, name: 'Cristiano Ronaldo'},
        {id: 12, name: 'Diego Maradona'},
        {id: 13, name: 'Manuel Neuer'},
        {id: 14, name: 'Wayne Rooney'}
    ],

    // Classes
    classes: [
        {id: 0, name: 'Geografia',  assessments: []},
        {id: 1, name: 'Matemática', assessments: []},
        {id: 2, name: 'História', assessments: []},
        {id: 3, name: 'Literatura',  assessments: []}
    ],

    assessments: [
        '2016-09-15',
        '2016-09-30',
        '2016-10-15',
        '2016-10-31'
    ]
};


function generateScores(className) {
    var items = [];
    classData.assessments.forEach(function(d) {
        items.push({
            date: new Date(d),
            score: 45 + Math.floor(Math.random() * 50 - 20 * Math.round(Math.random() < 0.5)),
            className: className
        });
    });

    return items;
}

function generateAbsences(from, to) {

    var items = [], dates = d3.time.days(from, to), dayOfWeek;

    dates.forEach(function(d) {
        dayOfWeek = d.getDay();
        if ((dayOfWeek !== 0) && (dayOfWeek !== 6) && (Math.random() < 0.05)) {
            items.push(d);
        }
    });

    return items;
}


// Gera ausências e notas
classData.students.forEach(function(d) {

    d.absences = generateAbsences(classData.from, classData.to);
    d.classes = [];

    classData.classes.forEach(function(u) {
        d.classes.push({
            name: u.name,
            scores: generateScores(u.name)
        });
    });

    // Calcula as médias
    var scores = d3.merge(d.classes.map(function(u) { return u.scores; }));
    d.avgScore = d3.mean(scores, function(u) { return u.score; });
});


classData.classes.forEach(function(d) {
    d.avgScores = [];
    classData.assessments.forEach(function(u) {

        d.avgScores.push({
            date: new Date(u),
            score: 45 + Math.floor(Math.random() * 50 - 20 * Math.round(Math.random() < 0.5))
        });

        d.avgScore = d3.mean(d.avgScores, function(m) { return m.score; });
    });
});

classData.weeks = [
    '2016-09-03',
    '2016-09-10',
    '2016-09-17',
    '2016-09-25',
    '2016-10-01',
    '2016-10-08',
    '2016-10-15',
    '2016-10-22',
    '2016-10-29'
];


classData.weeklyMetrics = [];
classData.weeks.forEach(function(d) {
    classData.weeklyMetrics.push({
        date: new Date(d),
        score: 45 + Math.floor(Math.random() * 50 - 20 * Math.round(Math.random() < 0.5)),
        absents: Math.floor(Math.random() * 4)
    });
});




