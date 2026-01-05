import { expect, test } from "@playwright/test";

test("Projects → Requirements → Tasks flow works in mock mode", async ({ page }) => {
  await page.setViewportSize({ width: 1280, height: 520 });
  await page.goto("/projects");

  await page.getByRole("heading", { name: "Projects" }).waitFor();
  await page.getByRole("button", { name: "Mock Project" }).click();
  await page.getByRole("link", { name: "Requirements" }).click();

  await page.getByText("Chat with the agent to generate requirements.").waitFor();
  await page.getByRole("textbox", { name: "Write a message…" }).fill("Generate requirements");
  await page.getByRole("button", { name: "Send" }).click();
  await expect(page.getByText("Business requirement 1")).toBeVisible();

  // Open focus edit: paste a valid Mermaid pie chart and ensure it renders as SVG (not as source).
  await page.getByRole("button", { name: "Focus edit" }).first().click();
  const pie = [
    "```mermaid",
    "pie title NETFLIX",
    '         "Time spent looking for movie" : 90',
    '         "Time spent watching it" : 10',
    "```",
  ].join("\n");
  await page
    .getByText("Description (markdown)")
    .locator("..")
    .locator("textarea")
    .fill(pie);
  await expect(page.locator('svg[role^="graphics-document"]').first()).toBeVisible();
  await page.getByRole("button", { name: "Close" }).click();
  await expect
    .poll(async () =>
      page.evaluate(
        () => document.querySelectorAll('[id^="dmermaid-"], [id^="mermaid-"]').length,
      ),
    )
    .toBe(0);

  // Browser page should not scroll; only internal panels should.
  await page.mouse.wheel(0, 2000);
  await expect.poll(async () => page.evaluate(() => window.scrollY)).toBe(0);
  const reqEditor = page.getByTestId("requirements-editor");
  const reqEditorDims = await reqEditor.evaluate((el) => ({
    scrollHeight: el.scrollHeight,
    clientHeight: el.clientHeight,
  }));
  expect(reqEditorDims.scrollHeight).toBeGreaterThanOrEqual(reqEditorDims.clientHeight);

  await page.getByRole("button", { name: "Confirm tasks" }).click();
  await page.getByText("Chat with the agent to generate a backlog.").waitFor();
  await expect(page.getByText("Epic 1")).toBeVisible();

  // accept a similar match and submit
  await page.getByRole("button", { name: "Accept" }).first().click();

  // Also verify focus edit in tasks doesn't leak Mermaid DOM nodes.
  await page.getByRole("button", { name: "Focus edit" }).first().click();
  await page.getByRole("button", { name: "Close" }).click();
  await expect
    .poll(async () =>
      page.evaluate(
        () => document.querySelectorAll('[id^="dmermaid-"], [id^="mermaid-"]').length,
      ),
    )
    .toBe(0);

  await page.getByRole("button", { name: "Submit to GitLab" }).click();
  await expect(page.getByText("Submitted to GitLab", { exact: true })).toBeVisible();

  // submission response should also be reflected in chat as a system message (markdown)
  await expect(page.getByText("GitLab submit")).toBeVisible();

  // verify internal scroll containers exist and page itself still doesn't scroll
  await page.mouse.wheel(0, 2000);
  await expect.poll(async () => page.evaluate(() => window.scrollY)).toBe(0);
  const chatThread = page.getByTestId("chat-thread");
  const chatDims = await chatThread.evaluate((el) => ({
    scrollHeight: el.scrollHeight,
    clientHeight: el.clientHeight,
  }));
  expect(chatDims.scrollHeight).toBeGreaterThanOrEqual(chatDims.clientHeight);

  const tasksEditor = page.getByTestId("tasks-editor-scroll");
  const tasksDims = await tasksEditor.evaluate((el) => ({
    scrollHeight: el.scrollHeight,
    clientHeight: el.clientHeight,
  }));
  expect(tasksDims.scrollHeight).toBeGreaterThanOrEqual(tasksDims.clientHeight);
});


