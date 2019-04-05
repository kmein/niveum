{ user }:
''
templates:
  params:
    author-name: ${user.name}
    author-email: ${user.email}
    copyright: 'Copyright: (c) 2019 ${user.name}'
    github-username: ${user.github}
''
