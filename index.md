---
layout: default
title: All Eagles 4th Down Seasons
---

# All Seasons

<ul>
  {% for page in site.pages %}
    {% if page.layout == "season" %}
      <li><a href="{{ site.baseurl }}{{ page.url }}">{{ page.title }}</a></li>
    {% endif %}
  {% endfor %}
</ul>
