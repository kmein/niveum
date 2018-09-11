{ fullName, fullEmail, githubUser }:
''
templates:
  params:
    author-name: ${fullName}
    author-email: ${fullEmail}
    category: Data
    copyright: 'Copyright: (c) 2018 ${fullName}'
    github-username: ${githubUser}
''
