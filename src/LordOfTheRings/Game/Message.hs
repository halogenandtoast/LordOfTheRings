module LordOfTheRings.Game.Message (module LordOfTheRings.Game.Message) where

import Data.List (mapAccumL)
import Data.Map.Strict qualified as Map (
  adjust,
  delete,
  filter,
  findWithDefault,
  insert,
  keys,
  lookup,
  member,
 )
import Data.UUID qualified as UUID (nil)
import LordOfTheRings.Ability.Types (
  Ability (abilityAnyPlayer, abilityIndex, abilitySource),
  abilityCost,
  isForcedAbility,
  isResponseAbility,
 )
import LordOfTheRings.ActiveCost (
  ActiveCost (activeCostCard, activeCostCost, activeCostDoomed),
  staticCost,
 )
import LordOfTheRings.Ally (
  AllyAttrs (
    allyAttacking,
    allyCardId,
    allyCommitted,
    allyExhausted,
    allyFacedown,
    allyOwner,
    allyPlacement,
    allyPoison,
    allyTokens,
    allyUnconscious
  ),
  createAlly,
  lookupAlly,
  newAlly,
 )
import LordOfTheRings.Attachment (
  AttachmentAttrs (
    attachmentCardId,
    attachmentController,
    attachmentExhausted
  ),
  lookupAttachment,
  newAttachment,
 )
import LordOfTheRings.Card (
  Card (EncounterCard, PlayerCard),
  CardDef (
    cdCardTraits,
    cdCardType,
    cdHitPoints,
    cdKeywords,
    cdLimitOneInVictoryDisplay,
    cdOtherSide,
    cdShadowEffect,
    cdVictoryPoints,
    cdWhenRevealed
  ),
  CardType (
    AllyType,
    EncounterSideQuestType,
    EnemyType,
    EventType,
    HeroType,
    LocationType,
    ObjectiveAllyType,
    ObjectiveType,
    TreacheryType
  ),
  EncounterCard (MkEncounterCard, ecId),
  HasCardCode (toCardCode),
  HasCardDef (toCardDef),
  IsCard (..),
  Sphere (Neutral),
  isAttachmentType,
  lookupCardDef,
 )
import LordOfTheRings.Card.Synthetic (orcGuard, reanimatedDead)
import LordOfTheRings.Classes.Entity (
  Entity (overAttrs, toAttrs, toId),
  insertEntity,
 )
import LordOfTheRings.Classes.HasGame (
  HasGame (getGame),
  getsGame,
 )
import LordOfTheRings.Classes.HasQueue (
  HasQueue (pushAll),
  clearQueue,
  push,
  pushEnd,
  pushWhen,
  runQueueT,
  withQueue_,
 )
import LordOfTheRings.Effect (
  Effect (
    Effect,
    effectDuration,
    effectModifiers,
    effectSource,
    effectTarget
  ),
  EffectDuration (
    UntilEndOfAttack,
    UntilEndOfPhase,
    UntilEndOfPhaseOf,
    UntilEndOfRound,
    UntilSpent
  ),
 )
import LordOfTheRings.Enemy (
  EnemyAttrs (
    enemyCardId,
    enemyFacedown,
    enemyOwner,
    enemyPlacement,
    enemyShadowFaceUp,
    enemyShadowRevealed,
    enemyTokens
  ),
  createEnemy,
  lookupEnemy,
  newEnemy,
 )
import LordOfTheRings.Entities (
  Entities (entitiesQuests),
  alliesL,
  attachmentsL,
  contractsL,
  enemiesL,
  heroesL,
  locationsL,
  objectivesL,
  questsL,
  treacheriesL,
 )
import LordOfTheRings.Event (lookupEvent)
import LordOfTheRings.Frontend (
  Frontend (send),
  FrontendMessage (FlipCard),
 )
import LordOfTheRings.Game.Base (
  Area (
    areaHeldEntities,
    areaHeldModifiers,
    areaJoining,
    areaPlayers,
    areaStatus
  ),
  AreaStatus (AreaWaiting),
  AttackDestroyed (
    AttackDestroyed,
    attackDestroyedBy,
    attackDestroyedCard
  ),
  Game (
    gameActiveCosts,
    gameAdditionalPhases,
    gameAttackDestroyed,
    gameEffects,
    gameEntities,
    gameFortitudeTests,
    gameLeftPlay,
    gameLeftPlayLocations,
    gameModifiers,
    gamePeril,
    gamePhase,
    gameResumeAfter,
    gameRevealer,
    gameSeparation,
    gameSurgeChain
  ),
  Separation (separationAreas, separationFirstPlayer),
  activeCostsL,
  additionalPhasesL,
  allPlayers,
  areaLead,
  areasInTurnOrder,
  attackDestroyedL,
  damageDealtL,
  effectsL,
  entitiesL,
  firstPlayerL,
  focusArea,
  fortitudeTestsL,
  isFocusedArea,
  isolated,
  joinArea,
  lastCommittedWillpowerL,
  leftPlayL,
  leftPlayLocationsL,
  logBlocksL,
  outcomeL,
  perilL,
  phaseL,
  playersL,
  questionL,
  resumeAfterL,
  revealerL,
  roundL,
  scenarioL,
  seatsFrom,
  separationL,
  splitCards,
  surgeChainL,
 )
import LordOfTheRings.GameT (GameT, putGame)
import LordOfTheRings.Helpers.Attachment (
  attachDiscount,
  attachMatcherFor,
  attachmentHosts,
  cardHosts,
 )
import LordOfTheRings.Helpers.Character (
  escapeTestTotals,
  fortitudeTestTotals,
  getAttackStrength,
  getCommittedWillpower,
  getDefenseAgainst,
  getHitPoints,
  hideTestTotals,
 )
import LordOfTheRings.Helpers.Modifiers (
  doesNotExhaustToCommit,
  getModifiers,
  immuneToSource,
  resourcesBarredFrom,
 )
import LordOfTheRings.Helpers.Staging (
  getEnemyAttack,
  getEnemyDefense,
  getEnemyHitPoints,
  getEngagementCost,
  getLocationQuestPoints,
  getStagingThreat,
 )
import LordOfTheRings.Helpers.Window (
  abilityController,
  abilityKey,
  asPlayer,
  damageableHeroes,
  exhaustableCharacters,
  exhaustableHeroes,
  getActionAbilities,
  getWindowAbilities,
  payCost,
  playerCardController,
  travelableLocations,
  withinLimitFor,
 )
import LordOfTheRings.Hero (
  HeroAttrs (
    heroAttacking,
    heroCardId,
    heroCommitted,
    heroExhausted,
    heroFacedown,
    heroGuardedBy,
    heroOwner,
    heroPoison,
    heroTokens,
    heroUnconscious
  ),
  lookupHero,
  newHero,
 )
import LordOfTheRings.I18n (cardVar, ikey, ikeyWith, intVar)
import LordOfTheRings.Id (
  AllyId (AllyId, unAllyId),
  AttachmentId (AttachmentId),
  EnemyId (EnemyId, unEnemyId),
  HeroId (HeroId, unHeroId),
  ObjectiveId (unObjectiveId),
 )
import LordOfTheRings.Keyword (
  GrappleLocation (FewestEnemiesGrappled, MostEnemiesGrappled),
  Keyword (
    Ambush,
    Deep,
    DefenseKeyword,
    Doomed,
    FirstPlayerGainsControl,
    Grapple,
    Guarded,
    GuardedBy,
    Hide,
    Indestructible,
    Loot,
    Peril,
    Prowl,
    Ranged,
    Regenerate,
    Sailing,
    Sentinel,
    Surge,
    Toughness,
    Venom
  ),
  isGrapple,
 )
import LordOfTheRings.Location (
  LocationAttrs (
    locationCardId,
    locationLookedAt,
    locationPlacement,
    locationPosition,
    locationTokens
  ),
  createLocation,
  enterFaceUp,
  grottoDeckCard,
  isUncharted,
  locationDeckCard,
  newLocation,
  turnLocationOver,
 )
import LordOfTheRings.Location.Grid (adjacentPositions)
import LordOfTheRings.Log.Block (
  LogBlockKind (
    AbilityBlock,
    EnemyAttackBlock,
    PlayerAttackBlock,
    ResolutionBlock,
    RevealBlock,
    WhenRevealedBlock
  ),
 )
import LordOfTheRings.Log.Refs (cardTitle, sourceCardTitle)
import LordOfTheRings.Matcher (
  AllyMatcher (
    AllyControlledBy,
    AllyWithTrait,
    AttackingAlly,
    CommittedAlly,
    ExhaustedAlly,
    ReadyAlly
  ),
  AttachMatcher (PlayIntoPlayArea, PlayIntoStagingArea),
  EnemyMatcher (
    EnemyEngagedWith,
    EnemyGuardingACard,
    EnemyInStagingArea,
    EnemyWithId
  ),
  HeroMatcher (
    AnyHero,
    AttackingHero,
    CommittedHero,
    ExhaustedHero,
    HeroControlledBy,
    HeroWithTrait,
    ReadyHero
  ),
  LocationMatcher (IsActiveLocation, LocationInStagingArea),
  PlayerMatcher (AnyPlayer, PlayerAtThisStage, PlayerWithId),
  Query (select),
  attackDamaged,
  attackDestroyedACharacter,
  characterController,
  characterTraits,
  charactersControlledBy,
  currentQuestHas,
  discoverValue,
  getAflame,
  getAlly,
  getArcheryTotal,
  getChosenQuest,
  getCurrentQuest,
  getDamage,
  getEliminationLevel,
  getEnemy,
  getEngagementThreat,
  getHero,
  getLead,
  getLocation,
  getMainQuests,
  getMire,
  getPlayer,
  getPlayerOrder,
  getResourcesOfSphere,
  getSideQuests,
  getStageLead,
  getStagePlayers,
  getThreat,
  getTimeX,
  hasKeyword,
  heroPaysFor,
  immuneToPlayerCards,
  notOutOfPlay,
  selectAny,
  selectCharacters,
  shipEnemy,
  shipObjective,
  shipObjectivesOf,
  timeCountersIn,
 )
import LordOfTheRings.Message (
  EscapeTest (
    escapeCards,
    escapeCommitted,
    escapeOnFail,
    escapeOnPass,
    escapeParty,
    escapePlayer,
    escapeSource
  ),
  FortitudeTest (
    fortitudeCommitted,
    fortitudeDiscarded,
    fortitudeExtraCards,
    fortitudeIgnored,
    fortitudeOnFail,
    fortitudeOnPass,
    fortitudePlayer,
    fortitudeSource
  ),
  HideTest (HideTest, hideCommitted, hidePlayer, hideSource),
  Message (
    ActionWindow,
    AddFromGrottoDeck,
    AddFromUnchartedDeck,
    AddToEncounterDiscard,
    AddToStaging,
    AddToVictoryDisplay,
    AddUnderwaterFromGrottoDeck,
    AdditionalPhases,
    AdvanceQuest,
    AllyBecomesAttachment,
    AllyBecomesEnemy,
    AllyBecomesHero,
    AllyEnteredPlayUnderController,
    AmbushEngagementCheck,
    ArcheryDamage,
    ArrangeIslandMap,
    Ask,
    AskToResolveWhenRevealed,
    AttachCard,
    AttachFreedCard,
    AttachEncounterCardFromHand,
    AttachFromEncounterDiscard,
    AttachPoison,
    AttackDamage,
    AttackDefended,
    AttackEndedUnresolved,
    AttackResolved,
    AttackStagingAreaAlone,
    AttackWith,
    BeginPhase,
    BeginRound,
    ChangeHeroController,
    CheckDefeated,
    CheckEliminations,
    CheckRestricted,
    CheckShadowRaisedAttack,
    CheckWindows,
    ChooseAndDiscard,
    ChooseAndDiscardFromHand,
    ChooseAndExhaust,
    ChooseAndExhaustHero,
    ChooseAttackTarget,
    ChooseEnemyAttack,
    ClaimObjective,
    CloseLogBlock,
    CommitCharacters,
    CommitToQuest,
    CompleteQuest,
    CreateEffect,
    CreateEnemy,
    CreateEnemyEngaged,
    DamageAflameLocation,
    DamageAnotherCharacter,
    DealDamage,
    DealDamageDivided,
    DealDamageDividedAmong,
    DealShadowCard,
    DealShadowCardToCard,
    DealShadowCards,
    DeclareAsDefender,
    DeclareAttackers,
    DeclareDefenders,
    DeclareRangedAttackers,
    DeclareSentinels,
    DefendWith,
    DefendedByMany,
    Destroy,
    Discard,
    DiscardForFortitudeTest,
    DiscardForHideTest,
    DiscardFromHand,
    DiscardGroupResources,
    DiscardLoot,
    DiscardRandomFromHand,
    DiscardShadowCards,
    DiscardedTopOfEncounterDeck,
    Do,
    DoPhase,
    DoStep,
    DrawSackCard,
    DrawTreacheryIntoHand,
    EachDiscardAnyFromHand,
    EliminatePlayer,
    EncounterCardDrawn,
    EncounterCardEnteredHand,
    EndEffect,
    EndEffects,
    EndEffectsFrom,
    EndGame,
    EndPhase,
    EndRound,
    EnemyAttack,
    EnemyAttacks,
    EnemyBecomesHero,
    EnemyEngagementChecks,
    EngageEnemy,
    EngageOrcGuard,
    EngagementCheck,
    Exhaust,
    ExhaustToReduceCost,
    FailHideTest,
    FinishRevelation,
    Flip,
    FlipKeepingProgress,
    FocusArea,
    ForgetLeftPlay,
    GameOver,
    GiveFromHand,
    GivePoison,
    GrappleCard,
    GuardCard,
    GuardLocationBy,
    GuardObjective,
    HealDamage,
    HeroBecomesAlly,
    HeroBecomesEnemy,
    IfAttackDamaged,
    IfAttackDealtNoDamage,
    IfAttackDestroyed,
    IgnoreFortitudeEyeIcons,
    JoinArea,
    JoinAreas,
    JoinAtTravel,
    LeaveQuestStage,
    LookAtFacedownSide,
    MakeEscapeTest,
    MakeFortitudeTest,
    MakeHideTest,
    NextPhase,
    OpenLogBlock,
    OptionalActionWindow,
    OptionalEngagement,
    PaidCost,
    PayCost,
    PayDamageHeroCost,
    PayExhaustCharacterCost,
    PayExhaustHeroCost,
    PayForCard,
    PayToAttach,
    PlaceEnemy,
    PlaceInStagingAreaOf,
    PlaceLocation,
    PlaceObjective,
    PlaceProgressOnQuest,
    PlaceTimeCounters,
    PlaceTokens,
    PlaceUnderneath,
    PlayAlly,
    PlayAtNoCost,
    PlayAttachment,
    PlayAttachmentOn,
    PlayCard,
    PlayEvent,
    PlayerAttacks,
    PrepareIslandMap,
    ProgressCanceled,
    PutAllyIntoPlayUnder,
    PutAllyIntoStagingArea,
    PutAttachmentIntoPlay,
    PutEnemyIntoPlayEngaged,
    PutHeroIntoPlay,
    PutHeroIntoPlayAs,
    PutInDiscard,
    PutIntoPlayAsActiveLocation,
    PutLocationIntoPlay,
    PutObjectiveAllyIntoPlay,
    PutObjectiveAllyIntoPlayUnder,
    PutObjectiveHeroIntoPlayUnder,
    PutObjectiveIntoPlay,
    PutObjectiveIntoPlayAttached,
    PutObjectiveIntoPlayGuardedBy,
    PutOnBottomOfGrottoDeck,
    PutOnBottomOfOwnersDeck,
    PutOnBottomOfQuestDeck,
    PutQuestIntoPlay,
    PutShadowCardIntoPlay,
    PutShadowEnemyIntoPlay,
    PutShadowEnemyIntoStaging,
    PutShadowLocationIntoPlay,
    PutShadowTreacheryIntoPlay,
    PutSideQuestIntoPlay,
    QuestResolution,
    RaiseThreat,
    Ready,
    Reanimate,
    RedirectAttack,
    ReduceActiveCost,
    RefreshUpTo,
    RemoveFromDeck,
    RemoveFromDiscard,
    RemoveFromGame,
    RemoveFromHand,
    RemoveProgressFromQuest,
    RemoveShadowCard,
    RemoveTokens,
    ReplaceWithTopOfCavesDeck,
    ResetToStage,
    ResolveArchery,
    ResolveAttack,
    ResolveBoarding,
    ResolveCampaign,
    ResolveDeep,
    ResolveDiscover,
    ResolveEscapeTest,
    ResolveFortitudeTest,
    ResolveGrapple,
    ResolveGuardedPlayerCard,
    ResolveHideTest,
    ResolveHinder,
    ResolveSailingDiscard,
    ResolveShadow,
    ResolveShadowCard,
    ResolveShadowEffect,
    ResolveSurge,
    ResolveWhenRevealedOf,
    RespondFromDiscard,
    ResponseWindow,
    ReturnToBottomOfEncounterDeck,
    ReturnToHand,
    ReturnToSackDeck,
    ReturnToTopOfEncounterDeck,
    RevealCard,
    RevealEncounterCard,
    RevealShadowCard,
    Run,
    SailingTest,
    SeparatePlayers,
    SeparateStagingAreas,
    SetAsideEnemy,
    SetAsideHero,
    SetAsideObjective,
    SetCurrentQuest,
    SetFirstPlayer,
    SetObjectiveGuard,
    SetPeril,
    SetRevealer,
    Setup,
    ShuffleFromHandIntoOwnersDeck,
    ShuffleIntoEncounterDeck,
    ShuffleIntoGrottoDeck,
    ShuffleIntoOwnersDeck,
    ShuffleIntoQuestDeck,
    ShuffleIntoSackDeck,
    ShuffleIntoUnchartedDeck,
    ShuffleUnchartedLocations,
    SpendGroupResources,
    SpendResources,
    SpendResourcesChosen,
    Staging,
    StopAttacking,
    SurgeReveal,
    Surrender,
    SwapHero,
    SwapIslandMapPositions,
    TakeControl,
    TakePrisoner,
    TravelActions,
    TravelTo,
    TurnShadowCardFaceUp,
    UndefendedAttack,
    UndefendedDamage,
    UseAbility,
    UseCardAbility,
    UseHandAction,
    WhenRevealed,
    WhenSacked,
    WhileInPlay,
    WithTarget
  ),
  chooseOrRunOne,
  inLogBlock,
  playerWindow,
  questionLabel,
  questionLabelWith,
  spendChosen,
 )
import LordOfTheRings.Message.Lifted (
  declaredAttacker,
  drawCards,
  lowerThreatBy,
  raiseThreatFromDoomed,
  setUpFellowship,
  setUpTheOneRing,
 )
import LordOfTheRings.Message.Lifted.Queue (capture)
import LordOfTheRings.Modifier (
  Modifier (modifierSource, modifierType),
  ModifierType (
    AdditionalAttackAfterNextAttack,
    AdditionalStagingReveals,
    AfterEngagementChecks,
    AfterStagingStep,
    ArcheryDamageMayGoTo,
    ArcheryDamageStaysOnCharacter,
    ArcheryDamageToAllies,
    ArcheryDamageToShipObjectives,
    AttackDamageAddsStagingThreat,
    AttackDamageDiscardsRandomFrom,
    AttackDamageDiscardsRandomFromHandOf,
    AttackDamageHeals,
    AttackDamageModifiesDamagedUntilEndOfRound,
    AttackDamageModifiesUntilEndOfRound,
    AttackDamageRaisesThreatOf,
    AttackDamageRedirectedTo,
    AttackDamageRemovesProgress,
    AttackDamageTakesPrisoner,
    AttackModifier,
    AttackModifierIfUndefended,
    AttackableByAnyCharacter,
    AttackableInStagingAreaByRanged,
    AttackedByOneCharacterAtATime,
    AttackedThisPhase,
    Blank,
    BottomOfDeckAtEndOfPhase,
    CanAttackStagingAreaAlone,
    CanCommitWhileExhausted,
    CannotAttack,
    CannotAttackPlayer,
    CannotBeAttacked,
    CannotBeCurrentQuest,
    CannotBeDamagedByEffectsOf,
    CannotBeDamagedByPlayerCardEffects,
    CannotBeDefended,
    CannotBeHealed,
    CannotBeOptionallyEngagedBy,
    CannotBeOptionallyEngagedWhileOthersInStaging,
    CannotBePoisoned,
    CannotCommitCharacters,
    CannotCommitToFortitudeTests,
    CannotCommitToQuest,
    CannotDiscardFromHand,
    CannotEngagePlayer,
    CannotExhaust,
    CannotLoseFirstPlayerToken,
    CannotTakeCombatDamage,
    CannotTakeDamage,
    CannotTakeDamageExceptFrom,
    CannotTakeNonCombatDamage,
    CannotTakeNonQuestDamage,
    ControlReturnsAtEndOfPhase,
    CurrentQuest,
    DamageIfQuestSuccessful,
    DamageNextRevealedEnemy,
    DamagePlacedFirstOn,
    DamageRedirectedTo,
    CancelNextDamage,
    DamageReducedTo,
    DamagedWhileDefending,
    Defending,
    DiscardAtEndOfPhase,
    DiscardAtEndOfRound,
    DiscardNextRevealed,
    DiscardRandomToCommit,
    DoesNotReadyDuringRefresh,
    DrawsOnDestroyingEnemy,
    EliminatedAtEndOfRound,
    EliminatedWithEmptyDeck,
    EnemyAttacksStepComplete,
    EnemyEngagedThisPhase,
    EngagedAnEnemyThisRound,
    EngagedThisRound,
    EngagementCostAgainst,
    EscapeTestCommitLimit,
    ExhaustsCharactersDamagedByExcess,
    FailedFortitudeTestThisRound,
    FailedHideTestThisPhase,
    FailedHideTestThisRound,
    FewerStagingReveals,
    FirstPlayerRevealsForStaging,
    ForNextAttack,
    GainKeyword,
    GainVictory,
    GameEndsWith,
    HealLimit,
    HealLimitEachRound,
    HealLimitExceptFrom,
    HealedThisRound,
    IgnoreAmbush,
    IgnoreDeep,
    IgnoresDefenseOfWhenAttackingAlone,
    IgnoresDefenseWhenAttackingAlone,
    ImmuneToRangedDamage,
    LeavesPlayToAnyPlayersDeck,
    LowerThreatIfQuestSuccessful,
    MaxDamage,
    MayDeclareMultipleDefenders,
    MayDeclareUpToDefenders,
    MustBeTraveledTo,
    MustCommitAll,
    MustCommitToQuest,
    NextEnemyInStaging,
    NoEngagementChecks,
    NoEngagementChecksAgainst,
    NoThreatFromQuestingUnsuccessfully,
    NotInStagingArea,
    OnlyEngagedPlayerFights,
    OptionalEngagementCost,
    OptionallyEngagesAdditionalEnemy,
    ProgressAsDamageTo,
    ProgressBeyondActiveTo,
    ProgressFromQuestingThisPhase,
    ProgressIfQuestUnsuccessful,
    ProgressInsteadOfAttackDamage,
    ProgressLimit,
    QuestFailureDamagesInstead,
    QuestProgressCanceledBy,
    QuestedSuccessfullyThisPhase,
    QuestedUnsuccessfullyThisRound,
    RaisesThreatAtEndOfPhase,
    ReadiedByRefresh,
    ReadyUndamagedDefenders,
    RefreshAtMost,
    RefreshThreatRaise,
    RemoveFromGameAtEndOfRound,
    ResolvingEnemyAttacks,
    ReturnToHandAtEndOfPhase,
    ReturnToHandAtEndOfRound,
    ReturnToStagingAtEndOfCombat,
    ReturnToTopOfEncounterDeckAfterCombat,
    ShadowlessCardsGainEffect,
    ShuffleIntoDeckAtEndOfPhase,
    ShuffleIntoOwnersDeckAtEndOfRound,
    SkipPhase,
    SkipQuestPhase,
    SkipStagingStep,
    StagingEnemiesEngageFirstPlayerAtEndOfPhase,
    StagingStepBegun,
    StagingThreatBonus,
    StaysInPlayWhenDefeated,
    TakesProgressAfterOtherActiveLocations,
    TreacheryRevealedThisPhase,
    TreacheryRevealedThisRound,
    UndefendedDamageMayGoTo,
    UndefendedDamageTo
  ),
 )
import LordOfTheRings.Objective (
  ObjectiveAttrs (
    objectiveCardId,
    objectiveGuard,
    objectivePlacement,
    objectiveTokens
  ),
  newObjective,
 )
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Phase (
  Phase (
    CombatPhase,
    EncounterPhase,
    PlanningPhase,
    QuestPhase,
    RefreshPhase,
    ResourcePhase,
    TravelPhase
  ),
  nextPhase,
 )
import LordOfTheRings.Placement (
  Placement (..),
  placementController,
 )
import LordOfTheRings.Player.Runner (
  canPlay,
  deckL,
  discardL,
  discardableHand,
  handL,
  setAsideCardsL,
 )
import LordOfTheRings.Prelude hiding (first, many)
import LordOfTheRings.Quest (isSideQuest, newQuest, newSideQuest)
import LordOfTheRings.Question (
  Question (
    ChooseAmount,
    ChooseOne,
    ChooseOneAtATime,
    QuestionLabel,
    SearchCards
  ),
  UI (AbilityLabel, Label),
  targetLabel,
 )
import LordOfTheRings.Saga (
  campaignResolvesOn,
  revealsIndividually,
 )
import LordOfTheRings.Scenario (
  ScenarioAttrs (scenarioId),
  cavesDeckL,
  encounterDeckAfterHeldReset,
  encounterDeckL,
  encounterDiscardL,
  gainedVictoryPointsL,
  grottoDeckL,
  inResolutionL,
  isEvilCreaturesCard,
  orcGuardsL,
  setAsideL,
  unchartedDeckL,
  victoryDisplayL,
 )
import LordOfTheRings.Source (
  Source (
    AllySource,
    CardIdSource,
    EnemyAttackSource,
    EnemySource,
    GameSource,
    LocationSource,
    PlayerAttackSource,
    PlayerSource,
    TreacherySource
  ),
  Sourceable (toSource),
  isPlayerCardSource,
  isQuestSource,
  sourceTarget,
 )
import LordOfTheRings.Target (
  Target (
    AllyTarget,
    AttachmentTarget,
    CardIdTarget,
    ContractTarget,
    EnemyTarget,
    GameTarget,
    HeroTarget,
    LocationTarget,
    ObjectiveTarget,
    PlayerTarget,
    QuestTarget,
    TreacheryTarget
  ),
  Targetable (toTarget),
 )
import LordOfTheRings.Token (
  Token (Damage, Progress, Resource, TimeCounter),
  addTokens,
  countTokens,
 )
import LordOfTheRings.Trait (Trait (Grotto, Hazard))
import LordOfTheRings.Treachery (
  TreacheryAttrs (treacheryCardId, treacheryPlacement),
  createTreachery,
  newTreachery,
 )
import LordOfTheRings.Window (
  Timing (After),
  Window (Window),
  WindowType (
    AttackDealtDamage,
    Claimed,
    AttackUndefended,
    AttackedAlone,
    AttackedEnemy,
    AttackersDeclared,
    ChangedControl,
    CharacterDestroyed,
    CharacterDestroyedByAttack,
    CharacterDestroyedUnder,
    CommittedToEscapeTest,
    CommittedToQuest,
    DamagedByEnemyAttack,
    DealtDamage,
    DeclaredAsAttacker,
    DeclaredAsDefender,
    DefendedAttack,
    DefendedWithoutDamage,
    DiscardedForFortitudeTest,
    DiscoverResolved,
    EliminatedByThreat,
    EnemyAttacked,
    EnemyAttacking,
    EnemyCardDestroyedByAttack,
    EnemyDeclaredAsAttacker,
    EnemyDefeated,
    EnemyDestroyed,
    EnemyDestroyedByAttack,
    EnemyEngaged,
    EngagedEnemyDestroyedByAttack,
    EntersPlay,
    EntersStagingArea,
    Exhausted,
    FailedFortitudeTest,
    FailedHideTest,
    GainedResources,
    Healed,
    LocationFlipped,
    LookedAtEncounterDeck,
    OptionallyEngaged,
    PassedHideTest,
    PhaseEnds,
    PlacedProgressFromQuesting,
    ProgressPlaced,
    ProgressPlacedOnCurrentQuest,
    QuestDefeated,
    QuestResolved,
    QuestedSuccessfully,
    QuestedUnsuccessfully,
    Readied,
    Revealed,
    RoundBegins,
    RoundEnds,
    ShadowEffectAgainst,
    ShadowEffectResolved,
    ShadowCardRevealed,
    ShadowEffectTriggered,
    ShadowIncreasedAttack,
    StagingStepBegins,
    StagingStepEnds,
    ThreatRaisedByDoomed,
    ThreatRaisedByEncounter,
    Traveled
  ),
  mkAfter,
  mkWhen,
 )

import LordOfTheRings.Game.Areas (
  eachAreaThen,
  separate,
  separateStages,
  stageGroups,
 )
import LordOfTheRings.Game.Cards (
  allyEntersPlay,
  attachedEnemies,
  checkRestricted,
  dealsNoDamage,
  defendingEffect,
  discardAttached,
  discardResponses,
  discardShadowsOf,
  discardTarget,
  enteredPlayThisRound,
  entering,
  freeGuarded,
  gainedTraits,
  gainsController,
  guardedTypes,
  handResponses,
  hinders,
  holds,
  isRestricted,
  leftPlay,
  lowest,
  objectiveAllyController,
  playAttachment,
  playsBeforeCommitting,
  printedFor,
  refreshing,
  returnToEncounterDeck,
  toAnyPlayersDeck,
  toStagingArea,
  withKeyword,
 )
import LordOfTheRings.Game.Combat (
  actionWindows,
  attackDamage,
  attackerRoom,
  attackingEnemy,
  attacksAsIfEngaged,
  attacksOf,
  canAttack,
  canDefend,
  canFight,
  dealAssigned,
  divideDamage,
  engageableFromStaging,
  engagedByCost,
  engagedForAttack,
  engagedWithEach,
  exhaustToDefend,
  isPhantom,
  mayAttack,
  ownAttackers,
  readyAttackers,
  readyCharacters,
  resolveShadowEffect,
  seatAfter,
  stagingAttackers,
  withHitPointsLeft,
 )
import LordOfTheRings.Game.Dispatch (runEach)
import LordOfTheRings.Game.End (gameCannotEndWith)
import LordOfTheRings.Game.Payment (payFor)
import LordOfTheRings.Game.Query (
  discardAttachedToQuest,
  getPlayers,
  guardLocation,
  inPlay,
  isDamageNextRevealedEnemy,
  isDiscardNextRevealed,
  isDiscoverValueModifier,
  isPlayerAttack,
  isQuestOrLocation,
  mayTakeQuestingProgress,
  optionalEngagement,
  printedDoomed,
  progressFirst,
  retargetCharacter,
  revealedCard,
  takingProgressFirst,
 )

runGameMessage :: Message -> Game -> GameT Game
runGameMessage msg g = case msg of
  -- the rest of an attack does nothing once its enemy has left play
  _ | Just eid <- attackingEnemy msg, not (Map.member eid g.entities.enemies) -> pure g
  Run msgs -> do
    pushAll msgs
    pure g
  WhileInPlay t msgs -> do
    when (inPlay g t) $ pushAll msgs
    pure g
  IfAttackDestroyed eid msgs -> do
    whenM (attackDestroyedACharacter eid) $ pushAll msgs
    pure g
  -- the refresh phase's readying is over once the character has readied
  Ready t -> pure $ g & effectsL %~ filter (\e -> not (effectTarget e == t && effectModifiers e == [ReadiedByRefresh]))
  -- read off the damage its last attack dealt (see 'attackDamaged')
  IfAttackDamaged eid targets msgs -> do
    damaged <- attackDamaged eid
    when (any (\(t, n) -> n > 0 && t `elem` targets) damaged) $ pushAll msgs
    pure g
  IfAttackDealtNoDamage eid msgs -> do
    dealt <- sum . map snd <$> attackDamaged eid
    when (dealt == 0) $ pushAll msgs
    pure g
  OpenLogBlock block -> pure $ g & logBlocksL %~ (<> [block])
  -- an ally that has just left play answers its own leaving
  UseCardAbility _ (AllySource aid) _ _ | Just a <- Map.lookup aid (gameLeftPlay g) -> do
    a' <- runEach msg a
    pure $ g & leftPlayL . ix aid .~ a'
  ForgetLeftPlay aid -> pure $ g & leftPlayL %~ Map.delete aid
  -- a location that has left play answers its own leaving
  UseCardAbility _ (LocationSource lid) _ _ | Just l <- Map.lookup lid (gameLeftPlayLocations g), not (Map.member lid g.entities.locations) -> do
    l' <- runEach msg l
    pure $ g & leftPlayLocationsL . ix lid .~ l'
  -- and to what its answer reads back in later steps, as Slopes of Cirith Gorgor's
  -- discards once the encounter deck is shuffled
  DoStep _ (UseCardAbility _ (LocationSource lid) _ _) | Just l <- Map.lookup lid (gameLeftPlayLocations g), not (Map.member lid g.entities.locations) -> do
    l' <- runEach msg l
    pure $ g & leftPlayLocationsL . ix lid .~ l'
  DiscardedTopOfEncounterDeck (LocationSource lid) _ _ | Just l <- Map.lookup lid (gameLeftPlayLocations g), not (Map.member lid g.entities.locations) -> do
    l' <- runEach msg l
    pure $ g & leftPlayLocationsL . ix lid .~ l'
  CloseLogBlock bid -> pure $ g & logBlocksL %~ filter ((/= bid) . (.id))
  -- step 8 follows the quest's own setup: a nightmare setup card's instructions, as its
  -- "the game is now ready to begin" reads; step 9 a campaign card's setup that needs what
  -- the quest put into play, as Iârion, or the fleet, as the Dream-chaser campaign's boons
  Setup -> do
    pushAll [DoStep n Setup | n <- [1 .. 9]]
    pure g
  DoStep 3 Setup -> do
    firstPlayer <- uniform g.playerOrder
    -- The One Ring's Setup follows the placing of the starting heroes, its first player's
    -- choice once there is one (A Shadow in the East rules), and comes before the setup hand
    runQueueT do
      push $ SetFirstPlayer firstPlayer
      setUpTheOneRing firstPlayer
      -- in a saga, the first player takes control of Bilbo Baggins or a Fellowship hero
      setUpFellowship firstPlayer
    pure g
  BeginRound -> do
    pushAll [CheckWindows [mkWhen RoundBegins], BeginPhase minBound, EndPhase minBound]
    -- no area has begun the round's first phase, those separated before the first round
    -- too, as at Unexpected Rescue
    pure $ g & roundL +~ 1 & separationL %~ fmap (\s -> s {separationAreas = [a {areaStatus = AreaWaiting} | a <- separationAreas s]})
  EndRound -> do
    -- each area ends the round in turn
    let (step, g') = eachAreaThen (const [EndRound]) id BeginRound g
    unless (isJust g.outcome) do
      -- a player who played Justice Shall Be Done this round leaves the game
      eliminated <- filterM (fmap (elem EliminatedAtEndOfRound) . getModifiers . PlayerTarget) (Map.keys g.players)
      pushAll $ map EliminatePlayer eliminated
      leaving <- filterM (fmap (elem DiscardAtEndOfRound) . getModifiers) (toList g.entities.allies)
      pushAll [Discard (toTarget a) | a <- leaving]
      removed <- filterM (fmap (elem RemoveFromGameAtEndOfRound) . getModifiers) (toList g.entities.heroes)
      pushAll [RemoveFromGame (toTarget h) | h <- removed]
      returning <- filterM (fmap (elem ReturnToHandAtEndOfRound) . getModifiers) (toList g.entities.allies)
      pushAll [ReturnToHand (toTarget a) | a <- returning, toId a `notElem` map toId leaving]
      -- a card passed by Message from Elrond, in a hand or in play
      let shuffled :: Targetable t => t -> GameT Bool
          shuffled = fmap (elem ShuffleIntoOwnersDeckAtEndOfRound) . getModifiers
      allies <- filterM shuffled (toList g.entities.allies)
      attachments <- filterM (shuffled . CardIdTarget . attachmentCardId . toAttrs) (toList g.entities.attachments)
      inHands <- filterM (shuffled . CardIdTarget . snd) [(pid, c.id) | (pid, p) <- mapToList g.players, c <- p.hand]
      pushAll
        $ [ShuffleIntoOwnersDeck (toTarget a) | a <- allies, toId a `notElem` map toId (leaving <> returning)]
        <> [ShuffleIntoOwnersDeck (toTarget a) | a <- attachments]
        <> [ShuffleFromHandIntoOwnersDeck pid cid | (pid, cid) <- inHands]
      -- each Aflame location in the staging area takes 1 damage (Raid on the Grey Havens)
      burning <- filterM (fmap (> 0) . getAflame) =<< select LocationInStagingArea
      pushAll [DealDamage GameSource (LocationTarget lid) 1 | lid <- burning]
      -- the round's lasting effects end once its end-of-round effects have resolved, which
      -- may read what happened this round, as Sneak Past the Guards
      pushAll [CheckWindows [mkWhen RoundEnds], EndEffects UntilEndOfRound]
      pushEnd step
    pure $ if isJust g.outcome then g' & effectsL %~ filter ((/= UntilEndOfRound) . effectDuration) else g'
  BeginPhase RefreshPhase -> do
    -- an enemy with regenerate heals as much damage, unless Saruman holds it out of play
    present <- filterM (notOutOfPlay . toTarget) (toList g.entities.enemies)
    regenerating <- for present \e -> do
      mods <- getModifiers e
      let printed = [n | Blank `notElem` mods, Regenerate n <- toList (cdKeywords $ toCardDef e)]
      pure [HealDamage (toSource e) (toTarget e) n | n <- printed <> [n | GainKeyword (Regenerate n) <- mods]]
    heroes <- filterM (fmap (notElem DoesNotReadyDuringRefresh) . getModifiers) =<< select ExhaustedHero
    allies <- select ExhaustedAlly
    players <- getPlayerOrder
    -- a player who may refresh no more characters than some, as Heart of Fangorn reads,
    -- chooses which, if they would refresh more
    limits <- for players \pid -> (pid,) . (\ms -> minimumMay [n | RefreshAtMost n <- ms]) <$> getModifiers (PlayerTarget pid)
    let characters = map toTarget heroes <> map toTarget allies
        controller = \case
          HeroTarget hid -> (.owner) <$> Map.lookup hid g.entities.heroes
          AllyTarget aid -> placementController . (.placement) =<< Map.lookup aid g.entities.allies
          _ -> Nothing
        limited =
          [ (pid, n, cs)
          | (pid, Just n) <- limits
          , let cs = [c | c <- characters, controller c == Just pid]
          , length cs > n
          ]
        held = concat [cs | (_, _, cs) <- limited]
    -- separated, the token passes once every area has refreshed: see 'eachAreaThen'
    keeps <- elem CannotLoseFirstPlayerToken <$> getModifiers (PlayerTarget g.firstPlayer)
    let passToken = case drop 1 players of
          (next : _) | not (isolated g), not keeps -> [SetFirstPlayer next]
          _ -> []
    raises <- for players \pid -> (pid,) . fromMaybe 1 . lastMay . (\ms -> [n | RefreshThreatRaise n <- ms]) <$> getModifiers (PlayerTarget pid)
    -- each character readied opens its own window, for responses to it readying
    readied <- for (filter (`notElem` held) characters) \c -> do
      traits <- characterTraits c
      pure [refreshing c, Ready c, CheckWindows [mkAfter $ Readied c traits]]
    pushAll
      $ concat regenerating
      <> concat readied
      <> [RefreshUpTo pid n cs | (pid, n, cs) <- limited]
      <> [Ready (toTarget a) | a <- toList g.entities.attachments, attachmentExhausted (toAttrs a)]
      <> [Ready (toTarget o) | o <- toList g.entities.objectives, o.exhausted]
      <> [Ready (toTarget c) | c <- toList g.entities.contracts, c.exhausted]
      <> [RaiseThreat pid n | (pid, n) <- raises, n > 0]
      <> passToken
      -- the phase ends with an action window, for Refresh Actions as Escape from Darkness gives
      <> map OptionalActionWindow players
    pure $ g & phaseL .~ RefreshPhase
  SetFirstPlayer pid -> do
    -- the first player gains control of Bilbo Baggins, or the Ring-bearer, as the token passes
    following <- filterM (\h -> hasKeyword FirstPlayerGainsControl (toTarget h) (toCardDef h)) (toList g.entities.heroes)
    pushAll [ChangeHeroController h.id pid | h <- following, h.owner /= pid]
    pure $ g & firstPlayerL .~ pid & separationL %~ fmap (\s -> s {separationFirstPlayer = pid})
  SetRevealer revealer -> pure $ g & revealerL .~ revealer
  SetPeril peril -> pure $ g & perilL .~ peril
  ChangeHeroController hid pid | Just h <- Map.lookup hid g.entities.heroes -> do
    let old = h.owner
        onHero a = a.placement == AttachedTo (toTarget hid)
    -- the player left with no hero, his last one gone to another, is eliminated; not for
    -- one lent until the end of the phase, as Desperate Alliance lends it (FAQ 1.9: "A
    -- player is not eliminated from the game unless each of his heroes is killed")
    lent <- elem (ControlReturnsAtEndOfPhase old) <$> getModifiers (HeroTarget hid)
    let othersLeft = any (\h' -> h'.owner == old && h'.id /= hid) (toList g.entities.heroes)
    pushWhen (not othersLeft && not lent) $ EliminatePlayer old
    pure
      $ g
      & entitiesL . heroesL . ix hid %~ overAttrs (\a -> a {heroOwner = pid})
      & entitiesL . attachmentsL . traverse %~ (\a -> if onHero a then overAttrs (\x -> x {attachmentController = Just pid}) a else a)
  SeparatePlayers groups -> pure $ separate groups g
  SeparateStagingAreas groups -> pure $ separateStages groups g
  FocusArea pid -> pure $ focusArea pid g
  -- while the players are separated, a card sent to another area's staging area leaves
  -- its own, what is attached to it going with it; sharing the game, one may come from
  -- any area's stage, or from a player's
  PlaceInStagingAreaOf pid target
    | Just s <- gameSeparation g
    , Just dest <- find (elem pid . areaPlayers) (separationAreas s)
    , held <- [a | a <- separationAreas s, not (isFocusedArea g a)]
    , Just from <- find (holds target . snd) ((Nothing, g.entities) : [(Just (areaPlayers a), areaHeldEntities a) | a <- held])
    , not (isFocusedArea g dest && isNothing (fst from)) -> do
        pushAll $ discardShadowsOf g target
        let (moving, staying) = splitCards [target] (snd from)
            moved = toStagingArea target moving
            fromHere a = Just (areaPlayers a) == fst from
            toHere a = areaPlayers a == areaPlayers dest && not (isFocusedArea g a)
            leave a = if fromHere a then a {areaHeldEntities = staying} else a
            arrive a = if toHere a then a {areaHeldEntities = areaHeldEntities a <> moved} else a
            here = if isNothing (fst from) then staying else g.entities
        pure
          $ g
          & entitiesL .~ (if isFocusedArea g dest then here <> moved else here)
          & separationL ?~ s {separationAreas = map (arrive . leave) (separationAreas s)}
  -- one already in that staging area stays there
  PlaceInStagingAreaOf _ target -> do
    case target of
      EnemyTarget eid | any ((/= StagingArea) . (.placement)) (Map.lookup eid g.entities.enemies) -> push $ PlaceEnemy eid StagingArea
      LocationTarget lid | any ((/= StagingArea) . (.placement)) (Map.lookup lid g.entities.locations) -> push $ PlaceLocation lid StagingArea
      ObjectiveTarget oid | any ((/= StagingArea) . (.placement)) (Map.lookup oid g.entities.objectives) -> push $ PlaceObjective oid StagingArea
      _ -> pure ()
    pure g
  -- a shadow card for an enemy known by its card, as one just put into play
  DealShadowCardToCard cid -> do
    for_ (find ((== cid) . enemyCardId . toAttrs) (toList g.entities.enemies)) \e -> push $ DealShadowCard (toId e)
    pure g
  JoinAtTravel ->
    pure $ g & separationL %~ fmap (\s -> s {separationAreas = [if isFocusedArea g a then a {areaJoining = True} else a | a <- separationAreas s]})
  JoinArea joining target -> pure $ joinArea joining target g
  -- in turn order each area whose quest card is defeated joins another, its active location
  -- discarded; with no other to join, all the players continue to the next stage together
  JoinAreas | Just s <- gameSeparation g -> do
    let ordered = areasInTurnOrder g s
        lead = areaLead g s
        entitiesOf a = if isFocusedArea g a then g.entities else areaHeldEntities a
        joinTo j t =
          FocusArea (lead j)
            : [Discard (toTarget l) | l <- toList (entitiesOf j).locations, l.placement == ActiveLocation]
            <> [JoinArea (lead j) (lead t)]
    case break areaJoining ordered of
      (_, []) -> pure ()
      (before, j : after) -> case filter (not . areaJoining) (before <> after) of
        [] ->
          pushAll
            $ concat [joinTo a j | a <- before <> after]
            <> [FocusArea (lead j)]
            <> take 1 [CompleteQuest (toId q) | q <- toList (entitiesOf j).quests, not (isSideQuest q)]
        [t] -> pushAll $ joinTo j t <> [JoinAreas]
        targets ->
          pushAll
            [ FocusArea (lead j)
            , questionLabel (ikey "label.chooseAreaToJoin") (lead j)
                [targetLabel (PlayerTarget $ lead t) (joinTo j t <> [JoinAreas]) | t <- targets]
            ]
    pure g
  RefreshUpTo pid n cs | n > 0, notNull cs -> do
    choices <- for cs \c -> do
      traits <- characterTraits c
      pure $ targetLabel c [refreshing c, Ready c, CheckWindows [mkAfter $ Readied c traits], RefreshUpTo pid (n - 1) (filter (/= c) cs)]
    push $ questionLabel (ikeyWith "label.refreshUpTo" [intVar "count" n]) pid choices
    pure g
  CheckEliminations -> do
    emptyDeck <- elem EliminatedWithEmptyDeck <$> getModifiers GameTarget
    eliminated <- forMaybeM (toList g.players) \player -> do
      heroes <- select (HeroControlledBy $ PlayerWithId player.id)
      level <- getEliminationLevel player.id
      -- nor for a hero lent to another player until the end of the phase, as by Desperate
      -- Alliance (FAQ 1.9)
      lent <- anyM (fmap (elem (ControlReturnsAtEndOfPhase player.id)) . getModifiers) (toList g.entities.heroes)
      -- a hero shuffled into the deck, as Lost and Alone does, is not killed (FAQ 2.37)
      -- a hero guarded by an encounter card is in play still, under no player's control, as
      -- Carried Away's: its owner is not eliminated for it
      let guarded = any (\h -> h.owner == player.id && isJust (heroGuardedBy $ toAttrs h)) g.entities.heroes
          -- nor for a hero turned to its enemy side, as Sméagol to Gollum: he is not killed,
          -- and the game goes on, as Lost and Alone's (The Lord of the Rings LCG Quest
          -- Companion FAQ, reported on the FFG forum, "Controlling No Heroes - When Do You
          -- Lose?")
          -- an ally turned enemy, as Secret Cultist's, is not a hero side
          turned = any (\e -> enemyOwner (toAttrs e) == Just player.id && cdCardType (toCardDef e) == EnemyType) g.entities.enemies
          noHeroes = null heroes && not guarded && not lent && not turned && not (any ((== HeroType) . (.cardType)) player.deck)
          byThreat = player.threat >= level
          -- Deadmen's Dike eliminates a player whose deck runs out
          byEmptyDeck = emptyDeck && null player.deck
      pure $ guard (not player.eliminated && (byThreat || noHeroes || byEmptyDeck)) $> (player.id, byThreat && not noHeroes)
    -- one reaching the elimination level opens a window first, as Favor of the Valar
    -- answers to stop it
    pushAll $ concat [[CheckWindows [mkWhen $ EliminatedByThreat pid] | byThreat] <> [EliminatePlayer pid] | (pid, byThreat) <- eliminated]
    pure g
  EliminatePlayer pid -> do
    heroes <- select (HeroControlledBy $ PlayerWithId pid)
    allies <- select (AllyControlledBy $ PlayerWithId pid)
    -- his cards leave play with him as his heroes and allies do, Gollum though engaged
    -- with another player
    let owned = [toId e | e <- toList g.entities.enemies, enemyOwner (toAttrs e) == Just pid]
    engaged <- filter (`notElem` owned) <$> select (EnemyEngagedWith $ PlayerWithId pid)
    let remaining = [p | p <- toList (allPlayers g), p.id /= pid, not p.eliminated]
        -- a separated player alone in his area takes it out of the game with him: the
        -- encounter cards there are discarded (ALeP 3.030), and it joins another area
        alone = case gameSeparation g of
          Just s | Just a <- find (elem pid . areaPlayers) (separationAreas s) -> null [p | p <- areaPlayers a, p /= pid, maybe False (not . (.eliminated)) (Map.lookup p (allPlayers g))]
          _ -> False
        others = [areaLead g s a | Just s <- [gameSeparation g], a <- areasInTurnOrder g s, not (isFocusedArea g a)]
        encounter =
          [toTarget e | e <- toList g.entities.enemies, e.placement == StagingArea]
            <> [toTarget l | l <- toList g.entities.locations, l.placement `elem` [StagingArea, ActiveLocation]]
        -- each location he controls returns to the staging area, its progress removed
        -- (Assault on Osgiliath and Wrath and Ruin rules sheets)
        controlled = [l | l <- toList g.entities.locations, l.placement == InPlayArea pid]
    pushAll
      $ concat [[RemoveTokens GameSource (toTarget l) Progress l.progress | l.progress > 0] <> [PlaceLocation (toId l) StagingArea] | l <- controlled]
      <> map (Discard . toTarget) heroes
      <> map (Discard . toTarget) allies
      <> map (Discard . toTarget) owned
      <> ( if alone
             then
               map (Discard . toTarget) engaged
                 <> if isJust (stageGroups g)
                   then [FocusArea pid, DoStep 1 (EliminatePlayer pid)]
                   else map Discard encounter <> [JoinArea pid other | other <- take 1 others]
             else [PlaceEnemy eid StagingArea | eid <- engaged]
         )
      <> [GameOver Lost | null remaining]
    pure g
  -- sharing the game, the stage of a player eliminated alone at it is discarded along
  -- with all the encounter cards there (The Road Darkens rules insert, "Multiple Staging
  -- Areas"), the stage in focus now
  DoStep 1 (EliminatePlayer pid) | Just s <- gameSeparation g -> do
    let encounter =
          [toTarget e | e <- toList g.entities.enemies, e.placement == StagingArea]
            <> [toTarget l | l <- toList g.entities.locations, l.placement `elem` [StagingArea, ActiveLocation]]
            <> [toTarget t | t <- toList g.entities.treacheries, t.placement == StagingArea]
        others = [areaLead g s a | a <- areasInTurnOrder g s, not (isFocusedArea g a)]
    pushAll $ map Discard encounter <> [JoinArea pid other | other <- take 1 others]
    pure g
  GameOver outcome -> do
    cannotEnd <- gameCannotEndWith outcome
    let s = toAttrs g.scenario
    if
      -- the campaign card's resolution under way ends the game once it has resolved
      | s.inResolution -> pure g
      -- the outcome waits while the game cannot end, unless no player is left to play on;
      -- see 'endWaitingGame'
      | cannotEnd && any (not . (.eliminated)) g.players ->
          g <$ push (CreateEffect $ Effect GameSource GameTarget [GameEndsWith outcome] UntilSpent)
      -- in campaign mode the campaign card's resolution runs first, as Arkham's scenario
      -- resolutions do: what it pushes runs, and may ask the players, then the game ends
      | campaignResolvesOn s.scenarioId outcome && isJust s.campaign -> do
          clearQueue
          resolution <- inLogBlock ResolutionBlock [ResolveCampaign]
          pushAll $ resolution <> [EndGame outcome]
          pure $ g & scenarioL %~ overAttrs (inResolutionL .~ True)
      | otherwise -> do
          clearQueue
          pure $ g & outcomeL ?~ outcome
  EndGame outcome -> do
    clearQueue
    pure $ g & outcomeL ?~ outcome
  -- conceding is not a loss by the rules, so nothing that stops the game ending stops it
  Surrender -> do
    clearQueue
    pure $ g & questionL .~ mempty & outcomeL ?~ Lost
  -- an ally put into play by its card, as Emery is, discarded by its card
  Discard (CardIdTarget cid)
    | Just a <- find ((== cid) . allyCardId . toAttrs) (toList g.entities.allies) ->
        g <$ push (Discard $ toTarget a)
  Discard target -> discardTarget target g
  -- damage that would be placed on the card goes first to another, as much as it still
  -- takes, the rest to this one, as City Wall takes Minas Tirith's
  DealDamage source target n
    | n > 0
    , (first, room) : _ <- [(t, k) | DamagePlacedFirstOn t k <- map modifierType (Map.findWithDefault [] target (gameModifiers g)), k > 0, inPlay g t] -> do
        let k = min n room
        pushAll $ DealDamage source first k : [DealDamage source target (n - k) | n > k]
        pure g
  DealDamage source target n | n > 0 -> do
    mods <- getModifiers target
    -- an enemy's attack's damage may go elsewhere, as Orc Saboteur's to a Battleground,
    -- not to one of those it already goes to
    attackRedirects <- case source of
      EnemyAttackSource eid -> (\ms -> [t | AttackDamageRedirectedTo t <- ms]) <$> getModifiers eid
      _ -> pure []
    -- the player whose card deals it, as Marshland Outlaws reads
    controller <- if isPlayerCardSource source then playerCardController source else pure Nothing
    -- damage another character takes in its place, as Song of Mocking's
    let redirected =
          ordNub
            $ filter (inPlay g)
            $ [t | DamageRedirectedTo t <- mods]
            <> (if target `elem` attackRedirects then [] else filter (/= target) attackRedirects)
    lead <- getLead
    -- before it is dealt, a Forced effect may cancel it, as Patrol Leader's: see 'cancelDamage'
    -- damage redirected is applied where it goes, not redirected again, as two Songs of
    -- Mocking each choosing the other's hero
    let dealTo t = do
          mods' <- getModifiers t
          damage <- getDamage t
          -- damage reduced to at most so much, as Nazgûl of Minas Morgul's to 1, and none past
          -- what the card can have, as City Guard's
          -- an enemy with toughness takes that much less each time it is assigned damage
          let toughness = case t of
                EnemyTarget eid
                  | Just e <- Map.lookup eid g.entities.enemies ->
                      sum $ [k | Blank `notElem` mods', Toughness k <- toList (cdKeywords $ toCardDef e)] <> [k | GainKeyword (Toughness k) <- mods']
                _ -> 0
              reduced = foldr min (n - toughness) $ [k | DamageReducedTo k <- mods'] <> [max 0 (k - damage) | MaxDamage k <- mods']
              -- the first effect canceling the next damage dealt to it cancels some, and ends
              canceling = take 1 [e | e <- gameEffects g, effectTarget e == t, notNull [() | CancelNextDamage _ <- effectModifiers e]]
              n' = reduced - sum [k | e <- canceling, CancelNextDamage k <- effectModifiers e]
              combat = case source of
                PlayerAttackSource _ -> True
                _ -> False
              barred =
                (CannotBeDamagedByPlayerCardEffects `elem` mods' && isPlayerCardSource source)
                  || maybe False (\p -> CannotBeDamagedByEffectsOf p `elem` mods') controller
                  || any (\s -> sourceTarget s /= sourceTarget source || isNothing (sourceTarget s)) [s | CannotTakeDamageExceptFrom s <- mods']
              blocked =
                CannotTakeDamage `elem` mods'
                  || reduced <= 0
                  || (CannotTakeNonCombatDamage `elem` mods' && not combat)
                  || (CannotTakeNonQuestDamage `elem` mods' && not (isQuestSource source))
                  || barred
          pure
            $ if blocked
              then []
              else map EndEffect canceling <> [m | n' > 0, m <- [CheckWindows [mkWhen $ DealtDamage t n'], DoStep 1 (DealDamage source t n')]]
    unless (CannotTakeDamage `elem` mods) $ pushAll =<< case redirected of
      [t] -> dealTo t
      -- the first player picks which takes it, as "dealt to 1 Morgul Bodyguard" reads
      _ : _ -> do
        choices <- for redirected \t -> targetLabel t <$> dealTo t
        pure [questionLabelWith (ikey "label.chooseDamageRedirect") (Just target) [] lead choices]
      [] -> dealTo target
    pure g
  DoStep 1 (DealDamage source target n) -> do
    -- the damage an enemy deals may raise a player's threat as much, as Seedy Inn's shadow
    attackMods <- case source of
      EnemyAttackSource eid -> getModifiers eid
      _ -> pure []
    let raising = [pid | AttackDamageRaisesThreatOf pid <- attackMods]
        -- each point of it removes 1 progress from each quest card, as Traitorous Wight's
        -- shadow reads
        removing =
          [ RemoveTokens source (toTarget q) Progress (min n q.progress)
          | AttackDamageRemovesProgress `elem` attackMods
          , q <- toList g.entities.quests
          , q.progress > 0
          ]
    defending <- elem Defending <$> getModifiers target
    -- a character an enemy with venom damages is given 1 poison (The Hobbit: On the
    -- Doorstep rules insert, "Venom, Poison, and Poisoned")
    venom <- case source of
      EnemyAttackSource eid | Just e <- Map.lookup eid g.entities.enemies -> hasKeyword Venom (toTarget eid) (toCardDef e)
      EnemySource eid | Just e <- Map.lookup eid g.entities.enemies -> hasKeyword Venom (toTarget eid) (toCardDef e)
      _ -> pure False
    let recorded = damageDealtL %~ (<> [(source, target, n)])
        character = case target of
          HeroTarget _ -> True
          AllyTarget _ -> True
          _ -> False
        poisoned = [GivePoison target 1 | venom, n > 0, character]
    pushAll
      $ [CreateEffect $ Effect GameSource target [DamagedWhileDefending] UntilEndOfAttack | defending]
      -- kept until the enemy attacks again, as what its attack destroyed is
      <> [ PlaceTokens source target Damage n
         , CheckWindows $ mkAfter (DealtDamage target n) : [mkAfter $ DamagedByEnemyAttack target eid | EnemyAttackSource eid <- [source]]
         , CheckDefeated source target
         ]
      <> poisoned
      -- a hero it leaves standing is taken prisoner, as Torture Master's shadow reads
      <> [TakePrisoner hid | AttackDamageTakesPrisoner `elem` attackMods, HeroTarget hid <- [target]]
      <> concat [[RaiseThreat pid n, CheckWindows [mkAfter $ ThreatRaisedByEncounter pid n]] | pid <- raising]
      <> [DiscardRandomFromHand pid n | AttackDamageDiscardsRandomFromHandOf pid <- attackMods]
      <> removing
      -- each point of it adds 1 to the staging area's threat this phase, as Ramming Speed!
      <> [CreateEffect $ Effect source GameTarget [StagingThreatBonus n] UntilEndOfPhase | AttackDamageAddsStagingThreat `elem` attackMods]
      <> [DiscardRandomFromHand pid n | AttackDamageDiscardsRandomFrom pid <- attackMods]
      <> [HealDamage source t n | AttackDamageHeals t <- attackMods]
      <> [CreateEffect $ Effect source t [m] UntilEndOfRound | n > 0, AttackDamageModifiesUntilEndOfRound t m <- attackMods]
      <> [CreateEffect $ Effect source target [m] UntilEndOfRound | n > 0, AttackDamageModifiesDamagedUntilEndOfRound m <- attackMods]
    pure $ recorded g
  GivePoison target n -> do
    barred <- elem CannotBePoisoned <$> getModifiers target
    controller <- if inPlay g target && not barred then characterController target else pure Nothing
    for_ (controller >>= \pid -> Map.lookup pid g.players) \p -> do
      let cards = [pc | PlayerCard pc <- take n p.deck]
      pushAll $ [RemoveFromDeck p.id pc.id | pc <- cards] <> [AttachPoison target cards | notNull cards]
    pure g
  HealDamage source target n | n > 0 -> do
    damage <- getDamage target
    mods <- getModifiers target
    -- a card is named by its title (rules reference), so "other than Athelas" spares the
    -- player attachment Athelas too
    let from = sourceCardTitle g source
        title code = cardTitle <$> lookupCardDef code
        healedThisRound = sum [k | HealedThisRound k <- mods]
        eachRound = [max 0 (k - healedThisRound) | HealLimitEachRound k <- mods]
        healed = foldr min (min n damage) $ [k | HealLimit k <- mods] <> [k | HealLimitExceptFrom k code <- mods, from /= title code] <> eachRound
    when (healed > 0 && CannotBeHealed `notElem` mods)
      $ pushAll
      $ [RemoveTokens source target Damage healed, CheckWindows [mkAfter $ Healed target source]]
      -- counted while a limit each round reads it
      <> [CreateEffect $ Effect source target [HealedThisRound healed] UntilEndOfRound | notNull eachRound]
    pure g
  ChooseAndDiscardFromHand pid source n | n > 0 -> do
    player <- getPlayer pid
    barred <- elem CannotDiscardFromHand <$> getModifiers (PlayerTarget pid)
    unless (barred || null (discardableHand player))
      $ push
      $ questionLabelWith
        (ikey "label.chooseCardToDiscardFromHand")
        (sourceTarget source)
        []
        pid
        [ targetLabel
            (CardIdTarget c.id)
            [DiscardFromHand pid c.id, ChooseAndDiscardFromHand pid source (n - 1)]
        | c <- discardableHand player
        ]
    pure g
  EachDiscardAnyFromHand source (pid : rest) discarded -> do
    player <- getPlayer pid
    push
      $ if null (discardableHand player)
        then EachDiscardAnyFromHand source rest discarded
        else
          questionLabelWith (ikey "label.discardAnyFromHand") (sourceTarget source) [] pid
            $ [ targetLabel (CardIdTarget c.id) [DiscardFromHand pid c.id, EachDiscardAnyFromHand source (pid : rest) (discarded <> [c])]
              | c <- discardableHand player
              ]
            <> [Label (ikey "label.doneDiscarding") [EachDiscardAnyFromHand source rest discarded]]
    pure g
  CheckDefeated source target -> do
    -- damage dealt in a batch can reach a card an earlier part of it already destroyed
    destroyed <- case target of
      HeroTarget hid | Map.member hid g.entities.heroes -> (>=) . (.damage) <$> getHero hid <*> getHitPoints target
      AllyTarget aid | Map.member aid g.entities.allies -> (>=) . (.damage) <$> getAlly aid <*> getHitPoints target
      -- an indestructible enemy, printed or gained as Daechanar's, keeps taking damage past
      -- its hit points; one that lost the keyword, as Thaurdir may, is destroyed as any other
      EnemyTarget eid | Just e <- Map.lookup eid g.entities.enemies -> do
        indestructible <- hasKeyword Indestructible target (toCardDef e)
        if indestructible then pure False else (>=) . (.damage) <$> getEnemy eid <*> getEnemyHitPoints eid
      -- an objective ally in the staging area, as Celador
      ObjectiveTarget oid | Just o <- Map.lookup oid g.entities.objectives, isJust (cdHitPoints $ toCardDef o) ->
        (>=) <$> getDamage target <*> getHitPoints target
      _ -> pure False
    when destroyed $ push (Destroy source target)
    pure g
  -- the When window first, while the enemy and what is attached to it are still in play,
  -- as Secret Vigil answers
  Destroy _ (EnemyTarget eid) | Map.member eid g.entities.enemies -> do
    pushAll [CheckWindows [mkWhen $ EnemyDefeated eid], DoStep 1 msg]
    pure g
  DoStep 1 (Destroy source target@(EnemyTarget eid)) | Map.member eid g.entities.enemies -> do
    enemy <- getEnemy eid
    stays <- elem StaysInPlayWhenDefeated <$> getModifiers target
    let ec = MkEncounterCard (enemyCardId $ toAttrs enemy) (toCardCode enemy)
        card = EncounterCard ec
        -- what a player's attack destroyed, for the attack to read as it resolves
        recorded = attackDestroyedL %~ (<> [AttackDestroyed source target Nothing card Nothing | PlayerAttackSource _ <- [source]])
    push $ CheckWindows [mkAfter $ EnemyDefeated eid, mkAfter $ EnemyDestroyed card]
    -- one that stays in play for its own effect was still destroyed by the attack
    gained <- headMay . (\ms -> [n | GainVictory n <- ms]) <$> getModifiers target
    -- one of the Evil Creatures deck goes to its discard pile, even with victory points
    -- (Journey Up the Anduin rules)
    let victory = (cdVictoryPoints (toCardDef enemy) <|> gained) <* guard (not $ isEvilCreaturesCard (toAttrs g.scenario) ec)
        gainedL = maybe id (\n -> gainedVictoryPointsL %~ Map.insert card.id n) (guard (isNothing $ cdVictoryPoints $ toCardDef enemy) *> gained)
    if stays then pure (recorded g) else recorded <$> case victory of
      Just _ -> do
        -- what is attached to it is discarded, an objective it guarded as a Key of The Three
        -- Trials, and a Mount, as Mordor Warg, goes to the staging area
        pushAll $ discardAttached g target
        pure
          $ g
          & entitiesL
          . enemiesL
          %~ Map.delete eid
          & scenarioL
          %~ overAttrs (gainedL . (victoryDisplayL %~ (card :)))
      Nothing -> discardTarget target g
  -- an objective ally destroyed in the staging area goes to the encounter discard pile
  Destroy _ (ObjectiveTarget oid) | Just o <- Map.lookup oid g.entities.objectives -> do
    push $ AddToEncounterDiscard [MkEncounterCard (objectiveCardId $ toAttrs o) (toCardCode o)]
    pure $ g & entitiesL . objectivesL %~ Map.delete oid
  Destroy source target -> do
    let (card, controller) = case target of
          HeroTarget hid | Just h <- Map.lookup hid g.entities.heroes -> (Just $ toCard h, Just h.owner)
          AllyTarget aid | Just a <- Map.lookup aid g.entities.allies -> (Just $ toCard a, placementController a.placement)
          _ -> (Nothing, Nothing)
        -- one an enemy's attack destroyed with its damage, as "if this attack destroys a
        -- character" reads: not one its shadow card's damage destroyed (see 'attackDestroyed')
        byAttack = [(eid, c, pid) | EnemyAttackSource eid <- [source], Just c <- [card], Just pid <- [controller]]
        under = [CharacterDestroyedUnder pid target | Just pid <- [controller]]
    pushAll
      $ [CheckWindows $ map mkWhen (CharacterDestroyed target c : under) | Just c <- [card]]
      <> [Discard target, CheckEliminations]
      <> [ CheckWindows
             $ mkAfter (CharacterDestroyed target c)
             : map mkAfter under
             <> [mkAfter $ CharacterDestroyedByAttack eid target pid | (eid, _, pid) <- byAttack]
         | Just c <- [card]
         ]
    let attacker eid = toCard <$> Map.lookup eid g.entities.enemies
    pure $ g & attackDestroyedL %~ (<> [AttackDestroyed (EnemyAttackSource eid) target (Just pid) c (attacker eid) | (eid, c, pid) <- byAttack])
  -- the heroes collect and the players draw, then each player has an action window, for
  -- Resource Actions as Captain's Wisdom's
  BeginPhase ResourcePhase -> do
    order <- getPlayerOrder
    pushAll $ DoPhase ResourcePhase : map OptionalActionWindow order
    pure $ g & phaseL .~ ResourcePhase
  BeginPhase PlanningPhase -> do
    players <- getPlayers
    pushAll [WithTarget (toTarget player) (DoPhase PlanningPhase) | player <- players]
    pure $ g & phaseL .~ PlanningPhase
  -- sharing the game split between staging areas, each area quests at its stage in turn
  BeginPhase QuestPhase -> do
    case stageGroups g of
      Just groups -> pushAll $ concat [[FocusArea lead, DoStep 1 (BeginPhase QuestPhase)] | (lead, _) <- groups] <> [FocusArea g.firstPlayer]
      Nothing -> push $ DoStep 1 (BeginPhase QuestPhase)
    pure $ g & phaseL .~ QuestPhase
  DoStep 1 (BeginPhase QuestPhase) -> do
    order <- getStagePlayers
    everyone <- getPlayerOrder
    -- the first player may choose a side quest in the staging area to quest against, but
    -- not a quest card that cannot be the current quest, as Secrets of Rhudaur
    let choosable = filterM (fmap (notElem CannotBeCurrentQuest) . getModifiers)
    mainQuests <- choosable =<< getMainQuests
    sideQuests <- choosable =<< getSideQuests
    let chooseQuest =
          [ questionLabel (ikey "label.chooseCurrentQuest") lead
            $ [Label (ikey "label.questAgainstMainQuest") [] | notNull mainQuests]
            <> [targetLabel q [SetCurrentQuest q] | q <- sideQuests]
          | notNull sideQuests
          , lead <- take 1 order
          ]
    skipped <- elem SkipQuestPhase <$> getModifiers GameTarget
    -- a stage with the Sailing keyword has the first player make a Sailing test before
    -- characters commit (Dream-chaser rules)
    let sailing = [SailingTest | any (\q -> not (isSideQuest q) && Sailing `member` cdKeywords (toCardDef q)) g.entities.quests]
    unless skipped $ pushAll $ sailing <> chooseQuest <> map CommitCharacters order <> [Staging] <> map ActionWindow everyone <> [QuestResolution]
    pure g
  BeginPhase TravelPhase -> do
    -- the players' actions come first, as Thrór's Map's, then the lead may travel
    -- sharing the game split between staging areas, each area may travel at its stage
    pushAll
      $ TravelActions
      : maybe
        [DoStep 1 (BeginPhase TravelPhase)]
        (\groups -> concat [[FocusArea lead, DoStep 1 (BeginPhase TravelPhase)] | (lead, _) <- groups] <> [FocusArea g.firstPlayer])
        (stageGroups g)
    pure $ g & phaseL .~ TravelPhase
  DoStep 1 (BeginPhase TravelPhase) -> do
    locations <- travelableLocations
    lead <- getStageLead
    forced <- filterM (fmap (elem MustBeTraveledTo) . getModifiers) locations
    unless (null locations)
      $ push
      $ if null forced
        then
          questionLabel (ikey "label.travelToLocation") lead
            $ [targetLabel lid [TravelTo lid] | lid <- locations]
            <> [Label (ikey "label.doNotTravel") []]
        else questionLabel (ikey "label.mustTravel") lead [targetLabel lid [TravelTo lid] | lid <- forced]
    pure g
  -- "when the players travel", as Crypt Key answers, comes before the location is active
  DoStep 1 (TravelTo lid) -> do
    pushAll [CheckWindows [mkWhen $ Traveled lid], PlaceLocation lid ActiveLocation, CheckWindows [mkAfter $ Traveled lid], ResolveDeep lid]
    pure g
  -- a Deep location, still the active location after the windows of traveling to it, is
  -- replaced by the top card of the Caves deck, unless that deck is empty (MEC65 rules)
  ResolveDeep lid | Just l <- Map.lookup lid g.entities.locations -> do
    deep <- hasKeyword Deep (toTarget l) (toCardDef l)
    ignored <- elem IgnoreDeep <$> getModifiers GameTarget
    pushWhen (deep && not ignored && l.placement == ActiveLocation && notNull (toAttrs g.scenario).cavesDeck)
      $ ReplaceWithTopOfCavesDeck lid
    pure g
  ResolveDeep _ -> pure g
  -- the Discover keyword: the player looks at the top X encounter cards, the discard pile
  -- shuffled in should the deck run short. He attaches a Loot objective among them to the
  -- location as a guarded objective, reveals one of their Hazard cards at random and adds
  -- it to the staging area, and discards the rest (MEC69 rules). A change to X made for
  -- this resolution, as Crypt Key's, is spent by it.
  ResolveDiscover pid lid | Just l <- Map.lookup lid g.entities.locations -> do
    value <- discoverValue (toTarget l) (toCardDef l)
    let spent e = effectDuration e == UntilSpent && effectTarget e == toTarget lid && any isDiscoverValueModifier (effectModifiers e)
        g' = g & effectsL %~ filter (not . spent)
        s = toAttrs g.scenario
    case value of
      Nothing -> pure g'
      Just x -> do
        let (top, rest) = splitAt x s.encounterDeck
        (looked, deck, discarded) <-
          if length top < x && notNull s.encounterDiscard
            then do
              shuffled <- shuffleM s.encounterDiscard
              let (more, rest') = splitAt (x - length top) shuffled
              pure (top <> more, rest', [])
            else pure (top, rest, s.encounterDiscard)
        hazard <- headMay <$> shuffleM (filter ((Hazard `member`) . cdCardTraits . toCardDef) looked)
        let loot = filter ((Loot `member`) . cdKeywords . toCardDef) looked
            resolve chosen =
              let others = [c | c <- looked, Just c /= chosen, Just c /= hazard]
               in [PutObjectiveIntoPlayGuardedBy c (toCard l).id | Just c <- [chosen]]
                    <> [RevealCard h | Just h <- [hazard]]
                    <> [AddToEncounterDiscard others | notNull others]
                    <> [CheckWindows [mkAfter $ DiscoverResolved lid (isJust hazard)]]
        pushWhen (notNull looked) $ CheckWindows [mkAfter $ LookedAtEncounterDeck pid]
        push $ case loot of
          _ : _ : _ ->
            Ask pid
              $ QuestionLabel (ikey "label.discoverChooseLoot") (Just $ toTarget lid) []
              $ SearchCards (map EncounterCard looked)
              $ ChooseOne [targetLabel c (resolve $ Just c) | c <- loot]
          [] | null looked -> Run $ resolve Nothing
          -- with no Loot to choose between, the player is still shown what he looked at
          _ ->
            Ask pid
              $ QuestionLabel (ikey "label.discoverLooked") (Just $ toTarget lid) []
              $ SearchCards (map EncounterCard looked)
              $ ChooseOne [Label (ikey "label.continue") (resolve $ headMay loot)]
        pure $ g' & scenarioL %~ overAttrs (\a -> a & encounterDeckL .~ deck & encounterDiscardL .~ discarded)
  ResolveDiscover _ _ -> pure g
  -- a Loot objective revealed from the encounter deck is discarded, and another card
  -- revealed in its place: it enters play only by the Discover keyword (MEC69 rules)
  DiscardLoot card -> g <$ pushAll [AddToEncounterDiscard [card], RevealEncounterCard]
  -- a location becomes the active location, opening its windows; a Dragon Sign, or another
  -- objective, goes to the staging area and resolves its guarded keyword (MEC66 rules)
  ReplaceWithTopOfCavesDeck lid -> case (toAttrs g.scenario).cavesDeck of
    card : rest -> do
      push $ Discard (LocationTarget lid)
      let g' = g & scenarioL %~ overAttrs (cavesDeckL .~ rest)
      case cdCardType (toCardDef card) of
        LocationType -> do
          thisLocation <- newLocation card
          push $ PlaceLocation (toId thisLocation) ActiveLocation
          pure $ g' & entitiesL . locationsL %~ insertEntity (overAttrs (\a -> a {locationPlacement = Unplaced}) thisLocation)
        t | t `elem` [ObjectiveType, ObjectiveAllyType] -> g' <$ push (PutObjectiveIntoPlay card)
        _ -> g' <$ push (AddToStaging card)
    [] -> pure g
  BeginPhase EncounterPhase -> do
    order <- getPlayerOrder
    -- players may act after the optional engagements and after the engagement checks;
    -- sharing the game split between staging areas, each area's players engage the enemies
    -- in its staging area in turn
    pushAll
      $ ( case stageGroups g of
            Nothing ->
              map OptionalEngagement order
                <> map OptionalActionWindow order
                <> [EngagementCheck pid 0 | pid <- take 1 order]
            Just groups ->
              concat
                [ FocusArea lead : map OptionalEngagement players <> map OptionalActionWindow order <> [EngagementCheck lead 0]
                | (lead, players) <- groups
                ]
                <> [FocusArea g.firstPlayer]
        )
      <> [CreateEffect $ Effect GameSource GameTarget [AfterEngagementChecks] UntilEndOfPhase]
      <> map OptionalActionWindow order
    pure $ g & phaseL .~ EncounterPhase
  -- a player who may engage an additional enemy, as Halbarad lets, is asked again once
  (optionalEngagement -> Just (pid, first)) -> do
    again <- (first &&) . elem OptionallyEngagesAdditionalEnemy <$> getModifiers (PlayerTarget pid)
    staged <- engageableFromStaging
    -- some enemies may only be engaged once they are alone in the staging area
    barredBy <- (\ms -> [m | CannotBeOptionallyEngagedBy m <- ms]) <$> getModifiers GameTarget
    resources <- getResourcesOfSphere pid Neutral
    enemies <- flip mapMaybeM staged \eid -> do
      mods <- getModifiers eid
      barred <- anyM (\m -> selectAny (PlayerWithId pid <> m)) $ barredBy <> [m | CannotBeOptionallyEngagedBy m <- mods]
      -- the resources it costs to engage, as Drúadan Stalker's 2
      let cost = sum [n | OptionalEngagementCost n <- mods]
      pure
        $ guard
          ( not barred
              && CannotEngagePlayer pid `notElem` mods
              && (length staged <= 1 || CannotBeOptionallyEngagedWhileOthersInStaging `notElem` mods)
              && cost <= resources
          )
        $> (eid, cost)
    unless (null enemies)
      $ push
      $ questionLabel (ikey "label.optionalEngagement") pid
      $ [ targetLabel eid
            $ [SpendResources pid Neutral cost | cost > 0]
            <> [EngageEnemy pid eid, CheckWindows [mkAfter $ OptionallyEngaged eid pid]]
            <> [DoStep 1 (OptionalEngagement pid) | again]
        | (eid, cost) <- enemies
        ]
      <> [Label (ikey "label.doNotEngage") []]
    pure g
  EngagementCheck pid seatsPassed -> do
    skip <- elem NoEngagementChecks <$> getModifiers GameTarget
    order <- getStagePlayers
    let next = seatAfter order pid
    threat <- getEngagementThreat pid
    candidates <- traverse (\eid -> (eid,) <$> getEngagementCost eid) =<< engageableFromStaging
    -- an enemy can be spared engagement checks against it, as a trap does
    checked <- filterM (fmap (\ms -> all (`notElem` ms) [NoEngagementChecks, CannotEngagePlayer pid, NoEngagementChecksAgainst pid]) . getModifiers . fst) candidates
    -- its cost against this player, as Hunting Pack's is lowered by his damaged characters
    costed <- for checked \(eid, cost) -> (\ms -> (eid, cost + sum [n | EngagementCostAgainst p n <- ms, p == pid])) <$> getModifiers eid
    case sortOn (Down . snd) [c | not skip, c@(_, cost) <- costed, cost <= threat] of
      ((eid, _) : _) -> pushAll [EngageEnemy pid eid, EngagementCheck next 0]
      [] -> when (seatsPassed + 1 < length order) $ push (EngagementCheck next (seatsPassed + 1))
    pure g
  AmbushEngagementCheck eid | Just enemy <- Map.lookup eid g.entities.enemies, enemy.placement == StagingArea -> do
    ambush <- hasKeyword Ambush (toTarget eid) (toCardDef enemy)
    gameMods <- getModifiers GameTarget
    pushWhen (ambush && IgnoreAmbush `notElem` gameMods) $ EnemyEngagementChecks eid
    pure g
  EnemyEngagementChecks eid | Just enemy <- Map.lookup eid g.entities.enemies, enemy.placement == StagingArea -> do
    gameMods <- getModifiers GameTarget
    mods <- getModifiers eid
    when (NoEngagementChecks `notElem` (gameMods <> mods)) do
      cost <- getEngagementCost eid
      -- its cost against each player, as for an engagement check
      let costFor p = cost + sum [n | EngagementCostAgainst p' n <- mods, p' == p]
      players <- traverse (\p -> (p,) <$> getEngagementThreat p) =<< getStagePlayers
      for_ (find (\(p, threat) -> threat >= costFor p && all (`notElem` mods) [CannotEngagePlayer p, NoEngagementChecksAgainst p]) players) \(p, _) -> push (EngageEnemy p eid)
    pure g
  EngageEnemy pid eid -> do
    -- a Ship enemy engaging from the staging area resolves its Boarding X (Dream-chaser rules)
    let fromStaging = maybe False ((== StagingArea) . (.placement)) (Map.lookup eid g.entities.enemies)
    pushAll
      $ [ CheckWindows [mkWhen $ EnemyEngaged eid pid]
        , PlaceEnemy eid (EngagedWith pid)
        ]
      <> [ResolveBoarding pid eid | fromStaging]
      <> [ CreateEffect $ Effect GameSource (PlayerTarget pid) [EngagedAnEnemyThisRound] UntilEndOfRound
         , CreateEffect $ Effect GameSource (EnemyTarget eid) [EngagedThisRound] UntilEndOfRound
         , CreateEffect $ Effect GameSource GameTarget [EnemyEngagedThisPhase] UntilEndOfPhase
         , CheckWindows [mkAfter $ EnemyEngaged eid pid]
         ]
    pure g
  BeginPhase CombatPhase -> do
    order <- getPlayerOrder
    -- sharing the game split between staging areas, archery is resolved at each stage
    let archery = maybe [ResolveArchery] (\groups -> concat [[FocusArea lead, ResolveArchery] | (lead, _) <- groups] <> [FocusArea g.firstPlayer]) (stageGroups g)
    pushAll
      $ archery
      <> ( ResolveHinder
             : DealShadowCards
             : map ActionWindow order
             <> map EnemyAttacks order
             -- the resolving enemy attacks step is complete, as Helm! Helm! reads
             <> [CreateEffect $ Effect GameSource GameTarget [EnemyAttacksStepComplete] UntilEndOfPhase]
             <> map PlayerAttacks order
         )
    pure $ g & phaseL .~ CombatPhase
  ResolveArchery -> do
    total <- getArcheryTotal
    pushWhen (total > 0) $ ArcheryDamage total []
    pure g
  -- each enemy with hinder engaged with a player removes 1 progress from the current quest,
  -- from the active location once the quest has none
  ResolveHinder -> do
    n <- length <$> (filterM (hinders g) =<< select (EnemyEngagedWith AnyPlayer))
    current <- getCurrentQuest
    let questProgress = sum [q.progress | q <- toList g.entities.quests, Just (toTarget q) == current]
        rest = n - min n questProgress
    pushWhen (min n questProgress > 0) $ RemoveProgressFromQuest GameSource n
    for_ (find ((== ActiveLocation) . (.placement)) (toList g.entities.locations)) \l ->
      pushWhen (rest > 0 && l.progress > 0) $ RemoveTokens GameSource (toTarget l) Progress (min rest l.progress)
    pure g
  -- the players divide it as they see fit, the first player having the final say, so the
  -- first player assigns it; it is all assigned, then dealt at once (ALeP 3.088)
  -- sharing the game split between staging areas, only the players at the stage assign
  -- it, to characters they control
  ArcheryDamage n assigned -> do
    lead <- getStageLead
    alliesOnly <- elem ArcheryDamageToAllies <$> getModifiers GameTarget
    let heroes = if isJust (stageGroups g) then HeroControlledBy PlayerAtThisStage else AnyHero
    -- the players deal it to characters they control, not to one no one does, as captured
    allies <- withHitPointsLeft assigned . map toTarget =<< select (AllyControlledBy PlayerAtThisStage)
    characters <-
      if alliesOnly && notNull allies
        then pure allies
        else withHitPointsLeft assigned =<< selectCharacters heroes (AllyControlledBy PlayerAtThisStage)
    gameMods <- getModifiers GameTarget
    -- Ship objectives take it all while they can, as City Under Siege reads
    ships <-
      if ArcheryDamageToShipObjectives `elem` gameMods
        then withHitPointsLeft assigned . map toTarget =<< select (shipObjective <> AllyControlledBy AnyPlayer)
        else pure []
    let staying = case assigned of
          c : _ | ArcheryDamageStaysOnCharacter `elem` gameMods, c `elem` characters -> [c]
          _ -> characters
        characters'
          | notNull ships = ships
          | otherwise = staying <> ordNub (filter (inPlay g) [t | ArcheryDamageMayGoTo t <- gameMods])
    push
      $ if n > 0 && notNull characters'
        then
          questionLabel (ikeyWith "label.assignArcheryDamage" [intVar "count" n]) lead
            $ [targetLabel c [ArcheryDamage (n - 1) (c : assigned)] | c <- characters']
        else Run $ dealAssigned GameSource assigned
    pure g
  DealShadowCards -> do
    order <- getPlayerOrder
    -- one engaged with each player from the staging area is dealt one for each attack
    -- an enemy with hinder is dealt none
    enemies <- filterM (\e -> not <$> orM [engagedWithEach e, hinders g e]) . ordNub =<< concatMapM attacksOf order
    pushAll $ map DealShadowCard enemies
    pure g
  EnemyAttacks pid -> do
    pushAll . (CreateEffect (Effect GameSource GameTarget [ResolvingEnemyAttacks] UntilEndOfPhase) :) . pure . ChooseEnemyAttack pid =<< attacksOf pid
    pure g
  ChooseEnemyAttack pid candidates -> do
    -- checked again before each attack: a card played between attacks may stop one
    engaged <- (<>) <$> select (EnemyEngagedWith $ PlayerWithId pid) <*> attacksAsIfEngaged pid
    enemies <-
      filterM
        (fmap (\ms -> CannotAttack `notElem` ms && CannotAttackPlayer pid `notElem` ms) . getModifiers)
        =<< filterM (fmap not . hinders g) [e | e <- candidates, e `elem` engaged]
    -- one engaged with each player is dealt a new shadow card for each attack, discarded after
    let attack eid = do
          each <- engagedWithEach eid
          pure $ [DealShadowCard eid | each] <> [EnemyAttack pid eid] <> [DiscardShadowCards eid | each]
    case enemies of
      [] -> pure ()
      [eid] -> pushAll =<< attack eid
      _ -> do
        choices <- for enemies \eid ->
          targetLabel eid . (<> [ChooseEnemyAttack pid (filter (/= eid) enemies)]) <$> attack eid
        push $ questionLabelWith (ikey "label.chooseNextAttack") Nothing (map toTarget enemies) pid choices
    pure g
  -- players may act at the end of each step of the attack: once its enemy is chosen,
  -- its defenders declared, its shadow resolved and its damage dealt
  EnemyAttack pid eid -> do
    windows <- actionWindows
    pushAll
      =<< inLogBlock
        (EnemyAttackBlock pid eid)
        (CheckWindows [mkWhen $ EnemyAttacking eid pid] : CheckWindows [mkAfter $ EnemyDeclaredAsAttacker eid pid] : windows <> [DeclareDefenders pid eid []])
    push $ CreateEffect $ Effect GameSource (EnemyTarget eid) [AttackedThisPhase] UntilEndOfPhase
    -- what its last attack destroyed or damaged is forgotten as it makes a new one
    pure
      $ g
      & attackDestroyedL %~ filter ((/= EnemyAttackSource eid) . attackDestroyedBy)
      & damageDealtL %~ filter (\(s, _, _) -> s /= EnemyAttackSource eid)
  DefendWith pid eid defender -> do
    windows <- actionWindows
    pushAll
      $ [ defendingEffect eid defender
        , CheckWindows [mkAfter $ DeclaredAsDefender defender eid]
        ]
      <> windows
      <> [ResolveShadow eid pid [defender]]
      <> windows
      <> [ AttackDamage eid defender
         , AttackDefended pid eid [defender]
         , CheckWindows [mkAfter $ EnemyAttacking eid pid, mkAfter $ DefendedAttack defender eid]
         ]
      <> windows
    pure g
  DeclareDefenders pid eid chosen -> do
    -- an attack that cannot be defended, as Striking Tentacle's may be, is undefended
    undefendable <- elem CannotBeDefended <$> getModifiers eid
    playerMods <- getModifiers (PlayerTarget pid)
    -- or up to so many, as Hold the Line lets
    let limit = minimumMay [n | MayDeclareUpToDefenders n <- playerMods]
        many = MayDeclareMultipleDefenders `elem` playerMods || isJust limit
        full = maybe False (length chosen >=) limit
    -- no other player's sentinel defends against one only its player fights
    alone <- elem OnlyEngagedPlayerFights <$> getModifiers eid
    others <- if alone then pure [] else filter (/= pid) <$> getPlayerOrder
    defenders <- if full then pure [] else filter (`notElem` chosen) <$> (filterM (canDefend eid) =<< readyCharacters pid)
    enemyName <- cardVar "enemy" <$> getEnemy eid
    exhausts <- for defenders \d -> (d,) <$> exhaustToDefend d eid
    let exhaust' d = [Exhaust d | Just True <- [lookup d exhausts]]
    push
      $ if
        | undefendable -> UndefendedAttack pid eid
        | many ->
          questionLabelWith (ikeyWith "label.declareDefenders" enemyName) (Just $ toTarget eid) chosen pid
            $ [ targetLabel d
                  $ exhaust' d
                  <> [ defendingEffect eid d
                     , CheckWindows [mkAfter $ DeclaredAsDefender d eid]
                     , DeclareDefenders pid eid (chosen <> [d])
                     ]
              | d <- defenders
              ]
            <> [ Label
                   (ikey $ if null chosen then "label.takeAttackUndefended" else "label.doneDeclaringDefenders")
                   -- sentinels may defend only once the attacked player declares no defenders
                   [DeclareSentinels pid eid chosen (if null chosen then others else [])]
               ]
        | otherwise ->
          questionLabelWith (ikeyWith "label.declareDefender" enemyName) (Just $ toTarget eid) [] pid
            $ [targetLabel d (exhaust' d <> [DefendWith pid eid d]) | d <- defenders]
            <> [Label (ikey "label.takeAttackUndefended") [DeclareSentinels pid eid [] others]]
    pure g
  DeclareAsDefender pid eid d -> do
    undefendable <- elem CannotBeDefended <$> getModifiers eid
    able <- canFight eid d
    unless (undefendable || not able) do
      pushAll [defendingEffect eid d, CheckWindows [mkAfter $ DeclaredAsDefender d eid]]
      -- the declaration still to come is the card's instead
      withQueue_ \q -> case break pending q of
        (before, DeclareDefenders _ _ chosen : after) -> before <> (DeclareSentinels pid eid (chosen <> [d]) [] : after)
        (before, DeclareSentinels _ _ chosen [] : after) -> before <> (DeclareSentinels pid eid (chosen <> [d]) [] : after)
        _ -> q
    pure g
   where
    pending = \case
      DeclareDefenders pid' eid' _ -> pid == pid' && eid == eid'
      DeclareSentinels pid' eid' _ [] -> pid == pid' && eid == eid'
      _ -> False
  -- then each other player, in turn, may defend with a sentinel of theirs: one, unless
  -- the attacked player may declare several
  DeclareSentinels pid eid chosen [] -> do
    windows <- actionWindows
    push $ case chosen of
      [] -> UndefendedAttack pid eid
      _ ->
        Run
          $ windows
          <> [ResolveShadow eid pid chosen]
          <> windows
          <> [ DefendedByMany pid eid chosen
             , AttackDefended pid eid chosen
             , CheckWindows $ mkAfter (EnemyAttacking eid pid) : [mkAfter $ DefendedAttack d eid | d <- chosen]
             ]
          <> windows
    pure g
  DeclareSentinels pid eid chosen (other : rest) -> do
    many <- elem MayDeclareMultipleDefenders <$> getModifiers (PlayerTarget pid)
    sentinels <-
      filterM (canDefend eid)
        . filter (`notElem` chosen)
        =<< withKeyword Sentinel
        =<< readyCharacters other
    enemyName <- cardVar "enemy" <$> getEnemy eid
    exhausts <- for sentinels \d -> (d,) <$> exhaustToDefend d eid
    let exhaust' d = [Exhaust d | Just True <- [lookup d exhausts]]
        defend d
          | many =
              exhaust' d
                <> [ defendingEffect eid d
                   , CheckWindows [mkAfter $ DeclaredAsDefender d eid]
                   , DeclareSentinels pid eid (chosen <> [d]) (other : rest)
                   ]
          | otherwise = exhaust' d <> [DefendWith pid eid d]
    push
      $ if null sentinels
        then DeclareSentinels pid eid chosen rest
        else
          questionLabelWith (ikeyWith "label.defendWithSentinel" enemyName) (Just $ toTarget eid) chosen other
            $ [targetLabel d (defend d) | d <- sentinels]
            <> [Label (ikey "label.doNotDefend") [DeclareSentinels pid eid chosen rest]]
    pure g
  DefendedByMany _ eid _ | dealsNoDamage g eid -> pure g
  DefendedByMany pid eid (filter (inPlay g) -> []) -> g <$ push (UndefendedDamage pid eid)
  DefendedByMany pid eid (filter (inPlay g) -> defenders) -> do
    attack <- getEnemyAttack eid
    defense <- sum <$> traverse (getDefenseAgainst eid) defenders
    choices <- for defenders \d -> targetLabel d <$> attackDamage pid eid d (attack - defense)
    -- with Phantom no defender takes it, so none is chosen
    phantom <- isPhantom eid
    push $ chooseOrRunOne pid (if phantom then take 1 choices else choices)
    pure g
  UndefendedAttack pid eid -> do
    windows <- actionWindows
    pushAll
      $ CheckWindows [mkAfter $ AttackUndefended pid eid]
      : windows
      <> [ResolveShadow eid pid []]
      <> windows
      <> [UndefendedDamage pid eid, CheckWindows [mkAfter $ EnemyAttacking eid pid]]
      <> windows
    pure g
  UndefendedDamage pid eid -> do
    attack <- (+) <$> getEnemyAttack eid <*> (sum . (\ms -> [n | AttackModifierIfUndefended n <- ms]) <$> getModifiers eid)
    -- the enemy's, or the player's, as guarding Gollum makes it
    redirected <-
      filter (inPlay g)
        . mapMaybe (\case UndefendedDamageTo t -> Just t; _ -> Nothing)
        <$> ((<>) <$> getModifiers eid <*> getModifiers (PlayerTarget pid))
    -- or a character the player may give it to instead of a hero, as White Tower Watchman
    watchmen <- filter (inPlay g) . (\ms -> [t | UndefendedDamageMayGoTo t <- ms]) <$> getModifiers (PlayerTarget pid)
    -- a Ship enemy's goes to a Ship objective the player controls (Dream-chaser rules)
    ship <- selectAny (EnemyWithId eid <> shipEnemy)
    ships <- if ship then map toTarget <$> shipObjectivesOf pid else pure []
    heroes <-
      if
        | notNull redirected -> pure []
        | notNull ships -> pure ships
        | otherwise -> (<> watchmen) . map toTarget <$> select (HeroControlledBy $ PlayerWithId pid)
    -- with several it may go to, as tied Battlegrounds at The Defense, the player chooses
    case ordNub redirected of
      [t] -> pushAll =<< attackDamage pid eid t attack
      ts@(_ : _) -> do
        options <- for ts \t -> targetLabel t <$> attackDamage pid eid t attack
        push $ questionLabelWith (ikey "label.undefendedDamage") (Just $ toTarget eid) [] pid options
      [] -> pure ()
    choices <- for heroes \h -> targetLabel h <$> attackDamage pid eid h attack
    phantom <- isPhantom eid
    unless (null heroes)
      $ push
      $ if phantom
        then chooseOrRunOne pid (take 1 choices)
        else questionLabelWith (ikey "label.undefendedDamage") (Just $ toTarget eid) [] pid choices
    pure g
  PlayerAttacks pid -> do
    enemies <- engagedByCost pid
    loners <-
      filterM (fmap (elem CanAttackStagingAreaAlone) . getModifiers)
        =<< select (HeroControlledBy $ PlayerWithId pid)
    ready <- readyAttackers pid
    raiders <- stagingAttackers ready
    ranged <- withKeyword Ranged ready
    -- a player card letting characters attack into the staging area, as Forth Eorlingas!,
    -- does not reach an enemy immune to player card effects (ruling)
    staged <-
      (<>)
        <$> (if notNull raiders then filterM (fmap not . immuneToPlayerCards . toTarget) =<< select EnemyInStagingArea else pure [])
        <*> (if notNull ranged then filterM (fmap (elem AttackableInStagingAreaByRanged) . getModifiers) =<< select EnemyInStagingArea else pure [])
    -- enemies anyone may attack, wherever they are, as one in an Ithilien Pit
    open' <-
      filterM (fmap (elem AttackableByAnyCharacter) . getModifiers)
        =<< select (not_ $ EnemyEngagedWith $ PlayerWithId pid)
    pushAll $ ChooseAttackTarget pid (ordNub $ enemies <> staged <> open')
      : [AttackStagingAreaAlone pid (toTarget h) | h <- loners]
    pure g
  -- the player chooses which enemy to attack next, each once, among those someone could
  ChooseAttackTarget pid candidates -> do
    targets <-
      filterM (fmap notNull . mayAttack pid) [eid | eid <- candidates, Map.member eid g.entities.enemies]
    case targets of
      [] -> pure ()
      [eid] -> push $ DeclareAttackers pid eid
      _ ->
        push
          $ questionLabelWith (ikey "label.chooseNextEnemyToAttack") Nothing (map toTarget targets) pid
          $ [ targetLabel eid [DeclareAttackers pid eid, ChooseAttackTarget pid (filter (/= eid) targets)]
            | eid <- targets
            ]
          <> [Label (ikey "label.doNotAttack") []]
    pure g
  AttackStagingAreaAlone pid attacker -> do
    ready <- elem attacker <$> readyAttackers pid
    staged <-
      filterM (\e -> andM [notElem CannotBeAttacked <$> getModifiers e, (> 0) . fromMaybe 1 <$> attackerRoom e])
        =<< select EnemyInStagingArea
    when (ready && notNull staged)
      $ push
      $ questionLabelWith (ikey "label.attackStagingAlone") (Just attacker) [] pid
      $ [ targetLabel
            eid
            $ [ AttackWith attacker
              , CreateEffect $ Effect GameSource attacker [AttackModifier 1] UntilEndOfAttack
              ]
            <> declaredAttacker attacker eid
            <> [CheckWindows [mkAfter $ AttackersDeclared eid], ResolveAttack pid eid]
        | eid <- staged
        ]
      <> [Label (ikey "label.doNotAttack") []]
    pure g
  DeclareAttackers pid eid -> do
    -- then other players' ranged characters may join an attack on an enemy engaged with them
    engaged <- engagedForAttack pid eid
    mods <- getModifiers eid
    let alone = OnlyEngagedPlayerFights `elem` mods
        -- one attacker only: once he is declared, no other joins him
        single = AttackedByOneCharacterAtATime `elem` mods
    declared <- notNull <$> selectCharacters AttackingHero AttackingAlly
    others <- if engaged && not alone && not single then filter (/= pid) <$> getPlayerOrder else pure []
    attackers <- if single && declared then pure [] else ownAttackers pid eid
    push
      $ if null attackers
        then DeclareRangedAttackers pid eid others
        else
          questionLabelWith (ikey "label.declareAttackers") Nothing [toTarget eid] pid
            $ [ targetLabel
                  a
                  (AttackWith a : declaredAttacker a eid <> [DeclareAttackers pid eid])
              | a <- attackers
              ]
            <> [Label (ikey "label.doneAttacking") [DeclareRangedAttackers pid eid others]]
    pure g
  -- the attack resolves, or its attackers stop, once no one else is left to ask; players
  -- may act once its attackers are declared and once its damage is dealt
  DeclareRangedAttackers pid eid others | null others || not (Map.member eid g.entities.enemies) -> do
    attackers <- selectCharacters AttackingHero AttackingAlly
    let attacked = notNull attackers && Map.member eid g.entities.enemies
    windows <- if attacked then actionWindows else pure []
    pushAll $ [CheckWindows [mkAfter $ AttackersDeclared eid] | attacked] <> windows <> [ResolveAttack pid eid] <> windows
    pure g
  DeclareRangedAttackers pid eid (other : rest) -> do
    attacking <- selectCharacters AttackingHero AttackingAlly
    ranged <-
      filterM (canAttack eid)
        . filter (`notElem` attacking)
        =<< withKeyword Ranged
        =<< readyAttackers other
    push
      $ if null ranged
        then DeclareRangedAttackers pid eid rest
        else
          questionLabelWith (ikey "label.joinAttackRanged") Nothing [toTarget eid] other
            $ [ targetLabel
                  a
                  (AttackWith a : declaredAttacker a eid <> [DeclareRangedAttackers pid eid (other : rest)])
              | a <- ranged
              ]
            <> [Label (ikey "label.doneAttacking") [DeclareRangedAttackers pid eid rest]]
    pure g
  -- the enemy is attacked, for its "when attacked" effects, before the damage is dealt
  ResolveAttack _ eid -> do
    attackers <- selectCharacters AttackingHero AttackingAlly
    pushAll $ [CheckWindows [mkWhen $ EnemyAttacked eid] | notNull attackers, Map.member eid g.entities.enemies] <> [DoStep 1 msg]
    pure g
  -- what the last attack on an enemy destroyed is forgotten as the next resolves
  DoStep 1 (ResolveAttack pid eid) -> do
    -- ranged characters of other players among them
    attackers <- selectCharacters AttackingHero AttackingAlly
    let unattached e = case e.placement of
          AttachedTo _ -> Nothing
          _ -> Just e
    case unattached =<< Map.lookup eid g.entities.enemies of
      -- the enemy left play, or became an attachment, before the attack resolved: the
      -- attackers just stop
      Nothing -> pushAll (map StopAttacking attackers)
      Just enemy' -> do
        mods <- getModifiers eid
        -- an enemy immune to ranged damage counts only the attackers of the player it is
        -- engaged with
        counted <-
          if ImmuneToRangedDamage `elem` mods
            then filterM (fmap (== Just (fromMaybe pid $ placementController enemy'.placement)) . characterController) attackers
            else pure attackers
        attack <- getAttackStrength counted eid (toCardDef enemy')
        alone <- case attackers of
          [a] -> do
            aMods <- getModifiers a
            orM
              [ pure $ IgnoresDefenseWhenAttackingAlone `elem` aMods
              , anyM (\m -> selectAny (EnemyWithId eid <> m)) [m | IgnoresDefenseOfWhenAttackingAlone m <- aMods]
              ]
          _ -> pure False
        defense <- if alone then pure 0 else getEnemyDefense eid
        hitPoints <- getEnemyHitPoints eid
        traits <- (cdCardTraits (toCardDef enemy') <>) <$> gainedTraits (toTarget eid)
        -- the attack may place progress in place of its damage, as The Passage Out reads
        asProgress <- elem ProgressInsteadOfAttackDamage <$> getModifiers GameTarget
        -- what a lone attack deals beyond the enemy's remaining hit points, as Firefoot reads
        let excess = if asProgress then 0 else max 0 $ attack - defense - (hitPoints - enemy'.damage)
            damaging = CannotTakeCombatDamage `notElem` mods && not asProgress
            -- what the attack deals an enemy that cannot have more than N damage, as City Guard
            dealt = foldr min (attack - defense) [max 0 (k - enemy'.damage) | MaxDamage k <- mods]
        if null attackers
          then pure ()
          else
            pushAll
              =<< inLogBlock
                (PlayerAttackBlock pid eid attackers)
                ( map StopAttacking attackers
                    <> [DealDamage (PlayerAttackSource pid) (toTarget eid) (attack - defense) | damaging]
                    <> [ PlaceTokens (PlayerSource pid) (toTarget eid) Progress (attack - defense)
                       | asProgress
                       , attack > defense
                       , CannotTakeCombatDamage `notElem` mods
                       ]
                    <> [ CheckWindows [mkAfter $ AttackDealtDamage eid attackers dealt]
                       | dealt > 0
                       , damaging
                       ]
                    <> [ AttackResolved eid (placementController enemy'.placement) attackers traits
                       ]
                    <> [ CheckWindows
                           $ [mkAfter $ AttackedEnemy a eid | a <- attackers]
                           <> [mkAfter $ AttackedAlone a eid excess | [a] <- [attackers]]
                       ]
                )
    pure $ g & attackDestroyedL %~ filter (not . isPlayerAttack . attackDestroyedBy)
  AttackResolved eid engaged attackers traits -> do
    -- only the attack's own damage destroying the enemy counts, not its leaving play some
    -- other way during the attack
    let byThisAttack d = d.target == EnemyTarget eid && isPlayerAttack (attackDestroyedBy d)
        destroyed = any byThisAttack $ gameAttackDestroyed g
    pushWhen destroyed $ CheckWindows $ mkAfter (EnemyDestroyedByAttack eid attackers traits)
      : [mkAfter $ EngagedEnemyDestroyedByAttack pid attackers | Just pid <- [engaged]]
      <> take 1 [mkAfter $ EnemyCardDestroyedByAttack eid (attackDestroyedCard d).id | d <- gameAttackDestroyed g, byThisAttack d]
    -- the attacking player draws for it, as Knife-work lets
    when destroyed do
      attacking <- ordNub . catMaybes <$> traverse characterController attackers
      for_ attacking \pid -> do
        mods <- Map.findWithDefault [] (PlayerTarget pid) <$> getsGame gameModifiers
        for_ [m.modifierSource | m <- mods, m.modifierType == DrawsOnDestroyingEnemy] \source ->
          runQueueT $ drawCards source pid 1
    pure $ g & effectsL %~ filter ((/= UntilEndOfAttack) . effectDuration)
  RedirectAttack eid target -> do
    let attacking = \case
          Window After (EnemyAttacking eid' _) -> eid' == eid
          _ -> False
    withQueue_ $ map \case
      AttackDamage eid' _ | eid' == eid -> AttackDamage eid target
      DefendedByMany _ eid' _ | eid' == eid -> AttackDamage eid target
      CheckWindows ws -> CheckWindows (filter (not . attacking) ws)
      m -> m
    pure g
  -- an attack that deals no damage, as Frodo Baggins (The Road Darkens) makes one
  AttackDamage eid _ | dealsNoDamage g eid -> pure g
  -- an attack turned on another enemy, as Small Target's, deals that enemy its damage
  AttackDamage eid target@(EnemyTarget other) -> do
    when (inPlay g target) do
      damage <- (-) <$> getEnemyAttack eid <*> getEnemyDefense other
      push $ DealDamage (EnemyAttackSource eid) target (max 0 damage)
    pure g
  -- a defender that left play before the damage leaves the attack undefended
  AttackDamage eid defender | not (inPlay g defender) -> do
    lead <- getLead
    engaged <- placementController . (.placement) <$> getEnemy eid
    push $ UndefendedDamage (fromMaybe lead engaged) eid
    pure g
  AttackDamage eid defender -> do
    attack <- getEnemyAttack eid
    defense <- getDefenseAgainst eid defender
    engaged <- placementController . (.placement) <$> getEnemy eid
    lead <- getLead
    pushAll =<< attackDamage (fromMaybe lead engaged) eid defender (attack - defense)
    pure g
  ResolveShadow eid pid defenders -> do
    -- a shadow card with no effect may be given one, as Mountains of Angmar gives
    gained <- elem ShadowlessCardsGainEffect <$> getModifiers GameTarget
    for_ (Map.lookup eid g.entities.enemies) \enemy -> do
      -- each shadow card turns face up, and one with an effect resolves it; one already face
      -- up resolved in an earlier attack, as an enemy attacking again has, unless it was
      -- dealt face up
      let resolving card =
            RevealShadowCard eid (ecId card)
              : CheckWindows [mkWhen $ ShadowCardRevealed eid (ecId card) pid defenders]
              : if gained || cdShadowEffect (toCardDef card)
                then
                  [ CheckWindows $ mkWhen (ShadowEffectTriggered (ecId card) eid)
                      : [mkWhen $ ShadowEffectAgainst (ecId card) d | d <- defenders]
                  , ResolveShadowCard (ecId card) eid pid defenders
                  ]
                else []
          pending =
            [ card
            | card <- enemy.shadowCards
            , card.id `notElem` (toAttrs enemy).enemyShadowRevealed || card.id `elem` (toAttrs enemy).enemyShadowFaceUp
            ]
      case pending of
        [] -> pure ()
        [card] -> pushAll (resolving card)
        -- they turn over one at a time, in the order the first player decides (ruling 3.388);
        -- this resolution goes on with the rest once that one has resolved
        _ -> do
          lead <- getLead
          push
            $ questionLabelWith (ikey "label.chooseShadowCard") (Just $ toTarget eid) [] lead
            $ [targetLabel (CardIdTarget card.id) (resolving card <> [ResolveShadow eid pid defenders]) | card <- pending]
    pure g
  RevealShadowCard eid cid -> do
    for_ (Map.lookup eid g.entities.enemies) \enemy ->
      when (any ((== cid) . ecId) enemy.shadowCards) $ send $ FlipCard cid
    pure g
  TurnShadowCardFaceUp eid cid -> do
    for_ (Map.lookup eid g.entities.enemies) \enemy ->
      when (any ((== cid) . ecId) enemy.shadowCards) $ send $ FlipCard cid
    pure g
  ResolveShadowCard cid eid pid defenders -> do
    for_ (Map.lookup eid g.entities.enemies) \enemy ->
      for_ (find ((== cid) . ecId) enemy.shadowCards) \card -> do
        push $ CheckWindows [mkAfter $ ShadowEffectResolved cid eid]
        push . CheckShadowRaisedAttack cid eid =<< getEnemyAttack eid
        resolveShadowEffect card (ResolveShadowEffect cid eid pid defenders)
    pure g
  -- the shadow effect has resolved: one that raised its enemy's attack opens a window, as
  -- Tides of Fate answers
  CheckShadowRaisedAttack cid eid before -> do
    when (Map.member eid g.entities.enemies) do
      after <- getEnemyAttack eid
      pushWhen (after > before) $ CheckWindows [mkWhen $ ShadowIncreasedAttack cid eid]
    pure g
  -- the card, in the discard pile, is built again to resolve its Forced effect, as a
  -- shadow card is
  ResolveSailingDiscard card _ -> do
    resolveShadowEffect card msg
    pure g
  -- a later step of a shadow effect, as "if this attack destroys a character" reads once
  -- the attack is over, which the shadow card, built again, resolves. The effect resolved
  -- as the card was revealed, so it still finishes once the card has been discarded, with
  -- its enemy leaving play after the damage (ALeP 3.043, 3.068); an attack its enemy left
  -- before the damage ended there (FAQ 2.54) and destroyed no one
  DoStep _ (ResolveShadowEffect cid eid _ _) -> do
    let dealt = maybe [] (.shadowCards) (Map.lookup eid g.entities.enemies)
    for_ (find ((== cid) . ecId) (dealt <> (toAttrs g.scenario).encounterDiscard)) \card -> resolveShadowEffect card msg
    pure g
  ShuffleIntoEncounterDeck (ObjectiveTarget oid) | Just o <- Map.lookup oid g.entities.objectives -> do
    deck <-
      shuffleM
        (MkEncounterCard (objectiveCardId $ toAttrs o) (toCardCode o) : (toAttrs g.scenario).encounterDeck)
    pure
      $ g
      & entitiesL
      . objectivesL
      %~ Map.delete oid
      & scenarioL
      %~ overAttrs (encounterDeckL .~ deck)
  -- from the encounter deck too, as The Fire Worm searches both
  AttachFromEncounterDiscard card host | card `elem` ((toAttrs g.scenario).encounterDiscard <> (toAttrs g.scenario).encounterDeck) -> do
    treachery <- overAttrs (\a -> a {treacheryPlacement = AttachedTo host}) <$> newTreachery card
    pure
      $ g
      & entitiesL
      . treacheriesL
      %~ insertEntity treachery
      & scenarioL
      %~ overAttrs ((encounterDiscardL %~ filter (/= card)) . (encounterDeckL %~ filter (/= card)))
  AttachEncounterCardFromHand pid card host -> do
    treachery <- overAttrs (\a -> a {treacheryPlacement = AttachedTo host}) <$> newTreachery card
    push $ RemoveFromHand pid card.id
    pure $ g & entitiesL . treacheriesL %~ insertEntity treachery
  PutShadowTreacheryIntoPlay host cid placement -> do
    for_ (Map.lookup host g.entities.enemies) \hostEnemy ->
      for_ (find ((== cid) . ecId) hostEnemy.shadowCards) \card -> do
        pushAll [RemoveShadowCard host cid]
        -- an enemy card dealt as a shadow card attaches as itself, as Dark Bats' shadow
        if cdCardType (toCardDef card) == EnemyType
          then do
            thisEnemy <- overAttrs (\a -> a {enemyPlacement = placement}) <$> newEnemy card
            putGame $ g & entitiesL . enemiesL %~ insertEntity thisEnemy
          else do
            thisTreachery <- overAttrs (\a -> a {treacheryPlacement = placement}) <$> newTreachery card
            putGame $ g & entitiesL . treacheriesL %~ insertEntity thisTreachery
    getGame
  ChooseAndExhaust source pid n | n > 0 -> do
    ready <-
      selectCharacters
        (HeroControlledBy (PlayerWithId pid) <> ReadyHero)
        (AllyControlledBy (PlayerWithId pid) <> ReadyAlly)
    unless (null ready)
      $ push
      $ questionLabelWith
        (ikey "label.chooseAndExhaustCharacter")
        (sourceTarget source)
        []
        pid
        [targetLabel c [Exhaust c, ChooseAndExhaust source pid (n - 1)] | c <- ready]
    pure g
  DealDamageDivided source pid n assigned -> do
    characters <- withHitPointsLeft assigned =<< charactersControlledBy pid
    divideDamage source pid n assigned characters (DealDamageDivided source pid)
    pure g
  DealDamageDividedAmong source pid among n assigned -> do
    characters <- withHitPointsLeft assigned $ filter (inPlay g) among
    if n > 0 && notNull characters
      then
        push
          $ questionLabelWith (ikeyWith "label.dealDamageDivided" [intVar "n" n]) (sourceTarget source) [] pid
            [targetLabel c [DealDamageDividedAmong source pid among (n - 1) (c : assigned)] | c <- characters]
      else pushAll $ dealAssigned source assigned
    pure g
  ChooseAndExhaustHero source pid -> do
    ready <- map toTarget <$> select (HeroControlledBy (PlayerWithId pid) <> ReadyHero)
    unless (null ready)
      $ push
      $ questionLabelWith
        (ikey "label.chooseAndExhaustHero")
        (sourceTarget source)
        []
        pid
        [targetLabel h [Exhaust h] | h <- ready]
    pure g
  PayExhaustHeroCost pid matcher rest -> do
    heroes <- asPlayer pid (exhaustableHeroes matcher)
    -- the ability paid for finds the hero in its windows
    let paidWith h = \case
          UseCardAbility p s i ws -> UseCardAbility p s i (ws <> [mkAfter $ Exhausted $ toTarget h])
          m -> m
    push
      $ questionLabelWith (ikey "label.chooseAndExhaustHero") Nothing [] pid
        [targetLabel h (Exhaust (toTarget h) : map (paidWith h) rest) | h <- heroes]
    pure g
  PayDamageHeroCost pid matcher n rest -> do
    heroes <- asPlayer pid (damageableHeroes matcher)
    push
      $ questionLabelWith (ikeyWith "label.dealDamageToOneHero" [intVar "n" n]) Nothing [] pid
        [targetLabel h (DealDamage (PlayerSource pid) (toTarget h) n : rest) | h <- heroes]
    pure g
  PayExhaustCharacterCost pid hm am rest -> do
    characters <- asPlayer pid (exhaustableCharacters hm am)
    let paidWith c = \case
          UseCardAbility p s i ws -> UseCardAbility p s i (ws <> [mkAfter $ Exhausted c])
          m -> m
    push
      $ questionLabelWith (ikey "label.chooseAndExhaustCharacter") Nothing [] pid
        [targetLabel c (Exhaust c : map (paidWith c) rest) | c <- characters]
    pure g
  -- the group pays, the player picking whose heroes' resources, 1 at a time
  SpendGroupResources pid sphere n | n > 0 -> g <$ push (SpendResourcesChosen pid True sphere n [])
  SpendResourcesChosen pid True sphere n chosen -> do
    heroes <- traverse getHero =<< select AnyHero
    payers <- filterM (heroPaysFor sphere) heroes
    push $ spendChosen pid True sphere n chosen [(h.id, h.resources) | h <- payers]
    pure g
  DiscardGroupResources pid n | n > 0 -> do
    heroes <- filter ((> 0) . (.resources)) <$> (traverse getHero =<< select AnyHero)
    unless (null heroes)
      $ push
      $ questionLabel (ikeyWith "label.discardMore" [intVar "count" n]) pid
      $ [ targetLabel h [RemoveTokens GameSource (toTarget h) Resource 1, DiscardGroupResources pid (n - 1)]
        | h <- heroes
        ]
    pure g
  DamageAflameLocation source pid -> do
    burning <- filterM (fmap (> 0) . getAflame) =<< select LocationInStagingArea
    pushWhen (notNull burning) $ chooseOrRunOne pid [targetLabel l [DealDamage source (LocationTarget l) 1] | l <- burning]
    pure g
  AllyEnteredPlayUnderController aid -> do
    for_ (Map.lookup aid g.entities.allies) \a -> case (toAttrs a).allyPlacement of
      InPlayArea pid -> push $ allyEntersPlay pid a
      _ -> pure ()
    pure g
  AttackEndedUnresolved _ -> pure $ g & effectsL %~ filter ((/= UntilEndOfAttack) . effectDuration)
  DamageAnotherCharacter source pid damaged -> do
    others <- filter (/= damaged) <$> charactersControlledBy pid
    -- each it damages is exhausted, as Terrible Strength reads: checked once the damage is
    -- dealt, against what the character had before
    exhausting <- case source of
      EnemyAttackSource eid -> elem ExhaustsCharactersDamagedByExcess <$> getModifiers eid
      _ -> pure False
    damage <- for others \c -> (c,) <$> getDamage c
    let deal c =
          DealDamage source c 1
            : [DoStep before (DamageAnotherCharacter source pid c) | exhausting, Just before <- [lookup c damage]]
    push $ case others of
      [] -> Run []
      [c] -> Run (deal c)
      _ ->
        questionLabelWith (ikey "label.damageAnotherCharacter") (sourceTarget source) [] pid
          $ [targetLabel c (deal c) | c <- others]
    pure g
  DoStep before (DamageAnotherCharacter _ _ c) -> do
    damaged <- if inPlay g c then (> before) <$> getDamage c else pure False
    pushWhen damaged $ Exhaust c
    pure g
  ChooseAndDiscard source pid targets0 -> do
    targets <- filterM (fmap not . immuneToSource source) targets0
    unless (null targets)
      $ push
      $ questionLabelWith
        (ikey "label.chooseCardToDiscard")
        (sourceTarget source)
        []
        pid
        [targetLabel t [Discard t] | t <- targets]
    pure g
  CommitCharacters pid -> do
    -- one that can commit while exhausted, as Lindon Navigator, need not be ready
    let commitsExhausted :: Targetable t => t -> GameT Bool
        commitsExhausted c = elem CanCommitWhileExhausted <$> getModifiers c
    heroes <-
      (<>)
        <$> select (HeroControlledBy (PlayerWithId pid) <> ReadyHero <> not_ CommittedHero)
        <*> (filterM commitsExhausted =<< select (HeroControlledBy (PlayerWithId pid) <> ExhaustedHero <> not_ CommittedHero))
    allies <-
      (<>)
        <$> select (AllyControlledBy (PlayerWithId pid) <> ReadyAlly <> not_ CommittedAlly)
        <*> (filterM commitsExhausted =<< select (AllyControlledBy (PlayerWithId pid) <> ExhaustedAlly <> not_ CommittedAlly))
    playerMods <- getModifiers (PlayerTarget pid)
    player <- getPlayer pid
    -- characters already committed have paid any toll for committing
    committed <-
      selectCharacters
        (HeroControlledBy (PlayerWithId pid) <> CommittedHero)
        (AllyControlledBy (PlayerWithId pid) <> CommittedAlly)
    let toll = [DiscardRandomFromHand pid 1 | DiscardRandomToCommit `elem` playerMods, null committed]
        barred = CannotCommitCharacters `elem` playerMods || (notNull toll && null player.hand)
        committable c = do
          mods <- getModifiers c
          free <- doesNotExhaustToCommit c
          pure $ CannotCommitToQuest `notElem` mods && (free || CannotExhaust `notElem` mods)
    characters <-
      if barred
        then pure []
        else
          filterM committable
            $ map toTarget heroes
            <> map toTarget allies
    quests <- getsGame (map toTarget . toList . entitiesQuests . gameEntities)
    -- a character's action may be worth more than its willpower, as Faramir's, so the
    -- player's actions are offered alongside committing it
    actions <- getActionAbilities pid
    mustCommitAll <- elem MustCommitAll <$> getModifiers GameTarget
    mustCommit <- filterM (fmap (elem MustCommitToQuest) . getModifiers) characters
    early <- filterM (\c -> andM [pure $ playsBeforeCommitting pid c, canPlay player c]) player.hand
    -- a player commits their characters at once, so responses to committing, as Aragorn's
    -- and Théodred's, wait until they are done and are then taken in any order
    -- asked even with no one left to commit, so the player moves on to staging
    push
      $ questionLabelWith (ikey "label.commitCharacters") Nothing quests pid
      $ [ targetLabel c (toll <> [CommitToQuest c, CommitCharacters pid])
        | c <- characters
        ]
      <> [AbilityLabel pid ab [] [CommitCharacters pid] | ab <- actions]
      <> [targetLabel (CardIdTarget c.id) [PlayCard pid c.id, CommitCharacters pid] | c <- early]
      <> [ Label (ikey "label.doneCommitting") [CheckWindows [mkAfter $ CommittedToQuest c | c <- committed] | notNull committed]
         | not mustCommitAll || null characters
         , -- a character that must commit, as A Fell Dread's hero, is committed first
           null mustCommit
         ]
    pure g
  Staging -> do
    push $ CreateEffect $ Effect GameSource GameTarget [StagingStepBegun] UntilEndOfPhase
    mods <- getModifiers GameTarget
    -- sharing the game split between staging areas, the players at the stage reveal
    order <- filterM (fmap (not . (.eliminated)) . getPlayer) =<< getStagePlayers
    let extra = sum [n | AdditionalStagingReveals n <- mods]
        fewer = sum [n | FewerStagingReveals n <- mods]
        players = length order
        -- the first player alone reveals so many instead, as The Battle of the Morannon
        -- has him reveal 1 for each resource on The Black Gate
        instead = headMay [n | FirstPlayerRevealsForStaging n <- mods]
        reveals = case instead of
          Just n -> max 0 $ n + extra - fewer
          Nothing -> max 0 $ max 1 (players + extra) - fewer
    -- a player revealing fewer, as Palantír of Orthanc has one
    playerFewer <- for order \pid -> (pid,) . (\ms -> sum [n | FewerStagingReveals n <- ms]) <$> getModifiers (PlayerTarget pid)
    -- in The Lord of the Rings saga each player reveals his own card in turn order, the
    -- first player any more the step reveals
    let withoutFewer = \case
          [] -> const []
          pid : rest -> \counts -> case lookup pid counts of
            Just n | n > 0 -> withoutFewer rest [(p, if p == pid then n - 1 else k) | (p, k) <- counts]
            _ -> pid : withoutFewer rest counts
        lead = fromMaybe g.firstPlayer $ headMay order
        revealers = case instead of
          Just _ -> replicate reveals lead
          Nothing -> withoutFewer (take reveals (order <> replicate reveals lead)) playerFewer
        revealing
          | revealsIndividually (scenarioId $ toAttrs g.scenario) =
              concat
                [ [SetRevealer (Just pid), RevealEncounterCard]
                | pid <- revealers
                ]
                <> [SetRevealer Nothing]
          | otherwise = replicate (max 0 $ reveals - sum (map snd playerFewer)) RevealEncounterCard
    -- what is played "after the staging step", as Elevenses, waits for its end
    pushAll
      $ ( if SkipStagingStep `elem` mods
            then []
            else
              CheckWindows [mkAfter StagingStepBegins]
                : revealing
                <> [CheckWindows [mkWhen StagingStepEnds]]
        )
      <> [CreateEffect $ Effect GameSource GameTarget [AfterStagingStep] UntilEndOfPhase]
    pure g
  QuestResolution -> do
    willpower <- getCommittedWillpower
    threat <- getStagingThreat
    players <- getStagePlayers
    -- the encounter deck tries to place progress on a stage with the Defense keyword: the
    -- players place none by questing, and raise no threat by failing (The Treason of
    -- Saruman rules insert, "Defense")
    defense <- currentQuestHas DefenseKeyword
    if
      | willpower > threat -> do
          -- a delayed effect resolves before the reactions to its moment, whether or not
          -- the card that made it is still in play (Rules Reference: Delayed Effects,
          -- Lasting Effects), as Bilbo Baggins's damage
          delayed <- for [(e, n, m) | e <- gameEffects g, DamageIfQuestSuccessful n m <- effectModifiers e] \(e, n, m) -> do
            guarding <- case effectTarget e of
              EnemyTarget eid -> selectAny (EnemyWithId eid <> EnemyGuardingACard)
              _ -> pure False
            pure $ DealDamage (effectSource e) (effectTarget e) (if guarding then m else n)
          lowered <-
            concat <$> for [(e, pid, n) | e <- gameEffects g, PlayerTarget pid <- [effectTarget e], LowerThreatIfQuestSuccessful n <- effectModifiers e] \(e, pid, n) ->
              capture $ lowerThreatBy (effectSource e) pid n
          -- what questing successfully triggers resolves before the progress is placed
          -- (FAQ 1.24), as The Hunt Begins' Forced effect
          pushAll
            $ CreateEffect (Effect GameSource GameTarget [QuestedSuccessfullyThisPhase] UntilEndOfPhase)
            : delayed
            <> lowered
            <> [CheckWindows [mkAfter QuestedSuccessfully]]
            <> (if defense then [] else [PlaceProgressOnQuest (willpower - threat), CheckWindows [mkAfter PlacedProgressFromQuesting]])
            <> [CheckWindows [mkAfter QuestResolved]]
      | threat > willpower ->
          -- responses before the threat is raised may stop it, as Ever Onward's: see
          -- 'doesNotRaiseThreat'
          do
            gameMods <- getModifiers GameTarget
            let spared = NoThreatFromQuestingUnsuccessfully `elem` gameMods
                -- the Assault keyword deals the difference to a card once for the group
                -- instead (MEC54 rules)
                damaged = [t | QuestFailureDamagesInstead t <- gameMods]
                delayed = [PlaceTokens (effectSource e) t Progress n | e <- gameEffects g, ProgressIfQuestUnsuccessful t n <- effectModifiers e]
            pushAll
              $ CreateEffect (Effect GameSource GameTarget [QuestedUnsuccessfullyThisRound] UntilEndOfRound)
              : CheckWindows [mkWhen QuestedUnsuccessfully]
              : delayed
              <> [RaiseThreat pid (threat - willpower) | not defense, not spared, null damaged, pid <- players]
              <> [PlaceProgressOnQuest (threat - willpower) | defense]
              <> take 1 [DealDamage GameSource t (threat - willpower) | t <- damaged, inPlay g t]
              <> [CheckWindows [mkAfter QuestedUnsuccessfully], CheckWindows [mkAfter QuestResolved]]
      | otherwise -> push $ CheckWindows [mkAfter QuestResolved]
    pure $ g & lastCommittedWillpowerL ?~ willpower
  PlaceTimeCounters target -> do
    x <- getTimeX target
    pushWhen (x > 0) $ PlaceTokens GameSource target TimeCounter x
    pure g
  PlaceTokens _ target Progress n | n > 0, isQuestOrLocation target -> do
    current <- (== Just target) <$> getCurrentQuest
    push $ CheckWindows $ mkAfter (ProgressPlaced target n) : [mkAfter $ ProgressPlacedOnCurrentQuest n | current]
    pure g
  -- tokens for a character by its card, as one put into play is known before it has an id
  PlaceTokens source (CardIdTarget cid) token n
    | Just h <- find ((== cid) . heroCardId . toAttrs) (toList g.entities.heroes) ->
        g <$ push (PlaceTokens source (toTarget h) token n)
    | Just a <- find ((== cid) . allyCardId . toAttrs) (toList g.entities.allies) ->
        g <$ push (PlaceTokens source (toTarget a) token n)
    | Just o <- find ((== cid) . objectiveCardId . toAttrs) (toList g.entities.objectives) ->
        g <$ push (PlaceTokens source (toTarget o) token n)
    -- an enemy that enters play with tokens, as Light Cruiser's boarders
    | Just e <- find ((== cid) . enemyCardId . toAttrs) (toList g.entities.enemies) ->
        g <$ push (PlaceTokens source (toTarget e) token n)
  -- the resource phase's resources come from the game, not a card effect
  PlaceTokens source target@(HeroTarget _) Resource n
    | n > 0
    , source /= GameSource -> do
        barred <- resourcesBarredFrom source target
        unless barred $ push $ CheckWindows [mkAfter $ GainedResources target]
        pure g
  -- an Orc Guard that would return to the staging area goes to its owner's discard pile
  -- instead (FAQ errata, Out of the Dungeons)
  PlaceEnemy eid StagingArea
    | Just e <- Map.lookup eid g.entities.enemies
    , toCardCode e == toCardCode orcGuard -> do
        push $ Discard (toTarget eid)
        pure g
  PlaceEnemy eid StagingArea -> do
    push $ CheckWindows [mkAfter $ EntersStagingArea (toTarget eid)]
    pure g
  PlaceProgressOnQuest n -> do
    actives <- select IsActiveLocation
    firsts <- if null actives then pure [] else takingProgressFirst
    if notNull firsts
      then g <$ (push =<< progressFirst GameSource n firsts \k -> [PlaceProgressOnQuest k | k > 0])
      else do
        -- what each active location still takes; one that takes only so much, as Perilous
        -- Swamp, is not explored by the rest, which is lost
        -- one that takes progress only after the others, as Doors of Durin, takes what
        -- they leave, the first player dividing nothing
        ranked <- for actives \lid -> do
          l <- getLocation lid
          remaining <- subtract l.progress <$> getLocationQuestPoints lid
          mods <- getModifiers lid
          let limits = [k | ProgressLimit k <- mods]
          pure (TakesProgressAfterOtherActiveLocations `elem` mods, (lid, remaining, max 0 $ foldr min remaining limits))
        let rooms = map snd (sortOn fst ranked)
            after = any fst ranked
        -- the rest goes to the side quest quested against, or the main quest chosen among
        -- several, else to the quest cards
        quests <- maybe getMainQuests (pure . pure) =<< getChosenQuest
        let place lid k = [PlaceTokens GameSource (toTarget lid) Progress k | k > 0]
        case rooms of
          -- with two active locations the first player divides the progress between them
          -- (FAQ 1.34); none passes on to the quest until both are explored
          [(a, _, roomA), (b, _, roomB)] | n < roomA + roomB, not after -> do
            lead <- getLead
            first <- getLocation a
            let lo = max 0 (n - roomB)
                hi = min n roomA
            push
              $ if lo == hi
                then Run (place a lo <> place b (n - lo))
                else
                  Ask lead
                    $ QuestionLabel (ikeyWith "label.divideProgress" (cardVar "location" first)) Nothing [toTarget a, toTarget b]
                    $ ChooseAmount lo [Label (ikeyWith "label.amount" [intVar "n" k]) (place a k <> place b (n - k)) | k <- [lo .. hi]]
          _ -> do
            let (left, onLocations) = mapAccumL (\m (lid, _, room) -> let k = min m room in (m - k, place lid k)) n rooms
                overflow = if all (\(_, remaining, room) -> room == remaining) rooms then left else 0
            -- what would go to the quest may be damage to an enemy instead, as Ride Them Down reads
            gameMods <- getModifiers GameTarget
            let instead = filter (inPlay g) [t | ProgressAsDamageTo t <- gameMods]
                -- or progress on a location, as Assault on Osgiliath's Nightmare setup card reads
                onto = filter (inPlay g) [t | ProgressBeyondActiveTo t <- gameMods]
                canceledBy q = find ((== Just q) . sourceTarget) [s | QuestProgressCanceledBy s <- gameMods]
                onQuests k =
                  concat
                    [ maybe
                        [CreateEffect $ Effect GameSource q [ProgressFromQuestingThisPhase] UntilEndOfPhase, PlaceTokens GameSource q Progress k]
                        (\s -> [ProgressCanceled s k])
                        (canceledBy q)
                    | q <- quests
                    , k > 0
                    ]
            -- a location in the staging area may take it before the quest, as Explorer's
            -- Almanac's, the first player choosing
            almanacs <- if overflow > 0 && null instead then mayTakeQuestingProgress else pure []
            lead <- getLead
            pushAll
              $ concat onLocations
              <> case instead of
                t : _ -> [DealDamage GameSource t overflow | overflow > 0]
                []
                  | t : _ <- onto -> [PlaceTokens GameSource t Progress overflow | overflow > 0]
                  | null almanacs -> onQuests overflow
                  | otherwise ->
                      [ questionLabelWith (ikey "label.questingProgress") Nothing (map (toTarget . fst) almanacs) lead
                          $ [ targetLabel lid (PlaceTokens GameSource (toTarget lid) Progress k : onQuests (overflow - k))
                            | (lid, room) <- almanacs
                            , let k = min overflow room
                            ]
                          <> [Label (ikey "label.placeOnQuest") (onQuests overflow)]
                      ]
        pure g
  PayForCard pid card fromHand windows
    -- a Guarded (X) attachment is not attached until it is free of encounters
    | isAttachmentType card.cardType
    , not (any (\case GuardedBy _ -> True; _ -> False) (cdKeywords $ toCardDef card)) -> do
        -- a host that lowers the cost is chosen before paying
        hosts <- cardHosts pid (toCardCode card)
        discounts <- traverse (attachDiscount card) hosts
        if any (> 0) discounts
          then
            g
              <$ push
                ( questionLabelWith
                    (ikey "label.attachTo")
                    (Just $ CardIdTarget card.id)
                    hosts
                    pid
                    [targetLabel h [PayToAttach pid card fromHand windows h] | h <- hosts]
                )
          else payFor g pid card fromHand windows Nothing 0
    | otherwise -> payFor g pid card fromHand windows Nothing 0
  PayToAttach pid card fromHand windows host -> payFor g pid card fromHand windows (Just host) =<< attachDiscount card host
  PaidCost costId -> do
    -- a card with Doomed, printed or gained, raises each player's threat as it is played,
    -- by its X less what lowers it, as Saruman's Staff
    for_ (Map.lookup costId (gameActiveCosts g)) \ac -> do
      gained <- getModifiers (CardIdTarget ac.activeCostCard.id)
      runQueueT
        $ raiseThreatFromDoomed
        $ printedDoomed ac.activeCostCard
        + sum [n | GainKeyword (Doomed n) <- gained]
        + ac.activeCostDoomed
    pure $ g & activeCostsL %~ Map.delete costId
  PlayAtNoCost _ card -> do
    runQueueT $ raiseThreatFromDoomed $ printedDoomed card
    pure g
  CreateEffect effect -> pure $ g & effectsL %~ (effect :)
  EndEffects duration -> pure $ g & effectsL %~ filter ((/= duration) . effectDuration)
  EndEffectsFrom source -> pure $ g & effectsL %~ filter ((/= source) . effectSource)
  EndEffect e -> pure $ g & effectsL %~ (\es -> let (before, after) = break (== e) es in before <> drop 1 after)
  AdditionalPhases phases -> pure $ g & additionalPhasesL %~ (phases <>)
  -- the round goes on from its own phase once the additional instances after it are done
  NextPhase phase -> case gameAdditionalPhases g of
    extra : rest -> do
      push $ Run [BeginPhase extra, EndPhase extra]
      pure $ g & additionalPhasesL .~ rest & resumeAfterL %~ (<|> Just phase)
    [] -> do
      -- a phase skipped, as The Great River skips the encounter phase, does not begin
      mods <- getModifiers GameTarget
      let following p = case nextPhase p of
            Just n | SkipPhase n `elem` mods -> following n
            n -> n
      push $ maybe EndRound (\next -> Run [BeginPhase next, EndPhase next]) (following $ fromMaybe phase (gameResumeAfter g))
      pure $ g & resumeAfterL .~ Nothing
  EndPhase phase -> do
    -- sharing the game split between staging areas, the cards at each stage see it end
    push $ case stageGroups g of
      Just groups -> Run $ concat [[FocusArea lead, CheckWindows [mkWhen $ PhaseEnds phase]] | (lead, _) <- groups] <> [FocusArea g.firstPlayer]
      Nothing -> CheckWindows [mkWhen $ PhaseEnds phase]
    -- an enemy still in play raises a player's threat, as Crazed Captive's shadow reads
    raising <- for (toList g.entities.enemies) \e -> (\ms -> [(p, n) | RaisesThreatAtEndOfPhase p n <- ms]) <$> getModifiers e
    pushAll $ concat [[RaiseThreat p n, CheckWindows [mkAfter $ ThreatRaisedByEncounter p n]] | (p, n) <- concat raising]
    -- each enemy in the staging area engages the first player, as The Ring Draws Them reads
    drawn <- elem StagingEnemiesEngageFirstPlayerAtEndOfPhase <$> getModifiers GameTarget
    when drawn do
      staging <- select EnemyInStagingArea
      pushAll [EngageEnemy g.firstPlayer e | e <- staging]
    -- at the end of each refresh phase each card with Time X loses 1 time counter, if able
    when (phase == RefreshPhase) do
      let es = g.entities
          cards = map toTarget (toList es.quests) <> map toTarget (toList es.locations) <> map toTarget (toList es.enemies)
      -- not a card Saruman holds out of play
      timed <- flip filterM cards \t -> andM [notOutOfPlay t, (&& timeCountersIn es t > 0) . (> 0) <$> getTimeX t]
      pushAll [RemoveTokens GameSource t TimeCounter 1 | t <- timed]
      -- and each location with Mire X in the staging area takes 1 mire token (The Land of
      -- Shadow rules insert; the revised Two Towers rulebook, p. 9)
      miring <- filterM (fmap (> 0) . getMire) =<< select LocationInStagingArea
      pushAll [PlaceTokens GameSource (LocationTarget lid) Resource 1 | lid <- miring]
    when (phase == CombatPhase) do
      returning <-
        filterM
          (fmap (elem ReturnToTopOfEncounterDeckAfterCombat) . getModifiers)
          (toList g.entities.enemies)
      pushAll [ReturnToTopOfEncounterDeck (toTarget e) | e <- returning]
      -- the enemies an effect returns to the staging area then, as Great Gray Wolf's
      matchers <- (\ms -> [m | ReturnToStagingAtEndOfCombat m <- ms]) <$> getModifiers GameTarget
      returningToStaging <- ordNub . concat <$> traverse select matchers
      pushAll [PlaceEnemy e StagingArea | e <- returningToStaging]
    -- an ally put into play until the end of the phase, as Command's, is discarded
    discarding <- flip filterM (toList g.entities.allies) \a ->
      elem DiscardAtEndOfPhase <$> getModifiers (CardIdTarget $ allyCardId $ toAttrs a)
    pushAll [Discard (toTarget a) | a <- discarding]
    -- one put into play until then, as To me! O my kinsfolk!'s, goes to the bottom of its
    -- owner's deck
    bottomed <- flip filterM (toList g.entities.allies) \a ->
      elem BottomOfDeckAtEndOfPhase <$> getModifiers (CardIdTarget $ allyCardId $ toAttrs a)
    pushAll [PutOnBottomOfOwnersDeck (toTarget a) | a <- bottomed]
    -- a hero given away until then, as Desperate Alliance gives one, goes back
    returned <- for (toList g.entities.heroes) \h -> map (h.id,) . (\ms -> [p | ControlReturnsAtEndOfPhase p <- ms]) <$> getModifiers h
    pushAll [ChangeHeroController hid p | (hid, p) <- concat returned]
    returning <- flip filterM (filter ((`notElem` map toId discarding) . toId) $ toList g.entities.allies) \a ->
      elem ReturnToHandAtEndOfPhase <$> getModifiers (CardIdTarget $ allyCardId $ toAttrs a)
    shuffling <- flip filterM (toList g.entities.allies) \a -> elem ShuffleIntoDeckAtEndOfPhase <$> getModifiers a
    -- an ally under both, as Beorn put into play by Sneak Attack, goes where the first
    -- player chooses (FAQ 1.02)
    let both = map toTarget $ filter ((`elem` map toId shuffling) . toId) returning
    lead <- getLead
    pushAll
      $ [ReturnToHand t | t <- map toTarget returning, t `notElem` both]
      <> [ShuffleIntoOwnersDeck t | t <- map toTarget shuffling, t `notElem` both]
      <> [ questionLabelWith
             (ikey "label.endOfPhaseAlly")
             (Just t)
             []
             lead
             [ Label (ikey "label.returnToOwnersHand") [ReturnToHand t]
             , Label (ikey "label.shuffleAllyIntoDeck") [ShuffleIntoOwnersDeck t]
             ]
         | t <- both
         ]
    -- separated, each area resolves the phase in turn before the next phase begins; the
    -- areas to join another do so at the beginning of the travel phase, and the token
    -- passes once every area has refreshed
    let following = NextPhase phase
        (step, g') =
          eachAreaThen
            (\status -> [BeginPhase phase | status == AreaWaiting] <> [EndPhase phase])
            (if phase == RefreshPhase then passToken else id)
            (if phase == QuestPhase then Run [JoinAreas, FocusArea (maybe g.firstPlayer separationFirstPlayer $ gameSeparation g), following] else following)
            g
        passToken s
          | keepsToken s = s
          | otherwise = s {separationFirstPlayer = fromMaybe s.separationFirstPlayer $ headMay $ drop 1 $ seatsFrom s.separationFirstPlayer g.playerOrder}
        -- unless he keeps it, as Captured by Spiders has the first player in his own area
        keepsToken s =
          or
            [ CannotLoseFirstPlayerToken `elem` map modifierType (Map.findWithDefault [] (PlayerTarget s.separationFirstPlayer) mods)
            | a <- separationAreas s
            , s.separationFirstPlayer `elem` areaPlayers a
            , let mods = if isFocusedArea g a then gameModifiers g else areaHeldModifiers a
            ]
    pushEnd step
    pure
      $ g'
      & effectsL %~ filter ((`notElem` [UntilEndOfPhase, UntilEndOfPhaseOf phase]) . effectDuration)
      & leftPlayLocationsL .~ mempty
      & damageDealtL %~ filter (\(s, _, _) -> case s of EnemyAttackSource _ -> True; _ -> False)
  PlayAlly pid card -> do
    thisAlly <- entering =<< newAlly pid card
    push $ enteredPlayThisRound thisAlly
    push $ allyEntersPlay pid thisAlly
    pure
      $ g
      & entitiesL
      . alliesL
      %~ insertEntity (overAttrs (\a -> a {allyPlacement = InPlayArea pid}) thisAlly)
  -- an ally in the staging area is under no player's control (Retreat from Osgiliath)
  PutAllyIntoStagingArea aid card -> do
    let thisAlly = overAttrs (\a -> a {allyPlacement = StagingArea}) $ createAlly card aid (fromMaybe g.firstPlayer card.owner)
    push $ CheckWindows [mkAfter $ EntersPlay $ toTarget thisAlly]
    pure $ g & entitiesL . alliesL %~ insertEntity thisAlly
  PutAllyIntoPlayUnder pid card -> do
    thisAlly <- entering =<< newAlly (fromMaybe pid card.owner) card
    runQueueT $ raiseThreatFromDoomed $ printedDoomed card
    push $ enteredPlayThisRound thisAlly
    push $ allyEntersPlay pid thisAlly
    pure
      $ g
      & entitiesL
      . alliesL
      %~ insertEntity (overAttrs (\a -> a {allyPlacement = InPlayArea pid}) thisAlly)
  -- attached as one played is, but not played
  -- with nothing it may attach to, it goes to its owner's discard pile, as an attachment
  -- freed of its guard does
  PutAttachmentIntoPlay pid card -> do
    runQueueT $ raiseThreatFromDoomed $ printedDoomed card
    hosts <- cardHosts pid (toCardCode card)
    let unattached = attachMatcherFor pid (toCardCode card) `elem` [PlayIntoStagingArea, PlayIntoPlayArea]
    push $ if null hosts && not unattached && isNothing (guardedTypes card) then PutInDiscard card else PlayAttachment pid card
    pure g
  PutSideQuestIntoPlay pid card -> do
    sideQuest <- newSideQuest pid card
    pure $ g & entitiesL . questsL %~ insertEntity sideQuest
  PutHeroIntoPlay pid card -> do
    thisHero <- newHero pid card
    push $ CheckWindows [mkAfter $ EntersPlay $ toTarget thisHero]
    pure $ g & entitiesL . heroesL %~ insertEntity thisHero
  PutHeroIntoPlayAs hid pid card -> do
    let thisHero = lookupHero (toCardCode card) card.id hid pid
    push $ CheckWindows [mkAfter $ EntersPlay $ toTarget thisHero]
    pure $ g & entitiesL . heroesL %~ insertEntity thisHero
  PutObjectiveHeroIntoPlayUnder pid card -> do
    thisHero <- lookupHero (toCardCode card) card.id <$> getRandom <*> pure pid
    push $ CheckWindows [mkAfter $ EntersPlay $ toTarget thisHero]
    pure $ g & entitiesL . heroesL %~ insertEntity thisHero
  CheckWindows windows -> do
    abilities <- getWindowAbilities windows
    let forcedAbilities = filter isForcedAbility abilities
    lead <- getLead
    -- while a Peril card's staging resolves, its player alone may respond
    order <- maybe getPlayerOrder (pure . pure) (gamePeril g)
    forcedMsgs <- case forcedAbilities of
      [] -> pure []
      [ab] -> do
        controller <- fromMaybe lead <$> abilityController ab
        pure [UseAbility controller ab windows]
      abs' -> pure [Ask lead $ ChooseOneAtATime [AbilityLabel lead ab windows [] | ab <- abs']]
    -- an enemy to attack again once this attack is over does, the effect spent
    let finished = [(eid, pid) | Window After (EnemyAttacking eid pid) <- windows]
        again e = case effectTarget e of
          EnemyTarget eid -> [(eid, pid) | AdditionalAttackAfterNextAttack pid <- effectModifiers e, (eid, pid) `elem` finished]
          _ -> []
        attacksAgain = concatMap again (gameEffects g)
    combat <- (== CombatPhase) <$> getsGame gamePhase
    pushAll
      $ forcedMsgs
      <> [ResponseWindow windows order [] 0]
      <> concat [[DealShadowCard eid, EnemyAttack pid eid] <> [DiscardShadowCards eid | not combat] | (eid, pid) <- attacksAgain]
    -- after an enemy's attack has resolved, what lasted for that attack ends
    let attackOver = any (\case Window After (EnemyAttacking _ _) -> True; _ -> False) windows
        -- an effect waiting for the next enemy in the staging area passes to it
        passed = nextAttack . case [eid | Window After (EntersStagingArea (EnemyTarget eid)) <- windows] of
          eid : _ -> map \e -> case [m | NextEnemyInStaging m <- effectModifiers e] of
            [] -> e
            ms -> e {effectTarget = EnemyTarget eid, effectModifiers = ms}
          [] -> id
        -- an effect waiting for the character's next attack lasts for that attack once it is
        -- declared as an attacker
        attackers = [t | Window After (DeclaredAsAttacker t _) <- windows]
        nextAttack = map \e -> case [m | ForNextAttack m <- effectModifiers e] of
          ms@(_ : _) | effectTarget e `elem` attackers -> e {effectModifiers = ms, effectDuration = UntilEndOfAttack}
          _ -> e
    pure $ g & effectsL %~ filter (null . again) . passed . (if attackOver then filter ((/= UntilEndOfAttack) . effectDuration) else id)
  ResponseWindow windows order used passes
    | passes < length order
    , (pid : _) <- order -> do
        abilities <- getWindowAbilities windows
        own <-
          filterM
            (\ab -> (== Just pid) <$> abilityController ab)
            [ab | ab <- abilities, isResponseAbility ab, not ab.abilityAnyPlayer, abilityKey ab `notElem` used]
        -- one any player may trigger, as Azain Silverbeard's, is offered to each, read as
        -- the player it is offered to
        anyone <-
          filterM (withinLimitFor pid)
            . filter (\ab -> isResponseAbility ab && ab.abilityAnyPlayer && abilityKey ab `notElem` used)
            =<< asPlayer pid (getWindowAbilities windows)
        let available = own <> anyone
        fromHand <- handResponses pid windows (ResponseWindow windows order used 0)
        -- a card responding from the discard pile stays there, so it is marked as used
        fromDiscard <- discardResponses pid windows used \cid -> ResponseWindow windows order ((CardIdSource cid, 0) : used) 0
        let next = ResponseWindow windows (drop 1 order <> [pid]) used (passes + 1)
        push
          $ if null available && null fromHand && null fromDiscard
            then next
            else
              playerWindow (ikey "label.useResponse") msg pid
                $ [ AbilityLabel pid ab windows [ResponseWindow windows order (abilityKey ab : used) 0]
                  | ab <- available
                  ]
                <> fromHand
                <> fromDiscard
                <> [Label (ikey "label.pass") [next]]
        pure g
  UseAbility pid ab windows -> do
    pushAll
      =<< inLogBlock (AbilityBlock pid ab.abilitySource (isForcedAbility ab))
      =<< payCost pid (abilityCost ab) [UseCardAbility pid ab.abilitySource ab.abilityIndex windows]
    pure g
  PlayEvent pid card _ -> do
    for_ (lookupEvent (toCardCode card) card.id pid) $ void . runEach msg
    pure g
  -- a later step of an event's effect, which the event, built again, resolves
  DoStep _ (PlayEvent pid card _) -> do
    for_ (lookupEvent (toCardCode card) card.id pid) $ void . runEach msg
    pure g
  -- the action of an ally in hand, which the ally, built from its card, resolves
  UseHandAction pid cid -> do
    player <- getPlayer pid
    for_ [pc | PlayerCard pc <- player.hand, pc.id == cid] \pc ->
      void $ runEach msg $ createAlly pc (AllyId UUID.nil) pid
    pure g
  RespondFromDiscard pid card _ -> do
    for_ (lookupEvent (toCardCode card) card.id pid) $ void . runEach msg
    when (cdCardType (toCardDef card) == AllyType) $ void $ runEach msg $ createAlly card (AllyId UUID.nil) pid
    pure g
  TravelActions -> do
    order <- getPlayerOrder
    pushAll $ map ActionWindow order
    pure g
  ReturnToHand target@(AllyTarget aid) | Just a <- Map.lookup aid g.entities.allies -> do
    g' <- leftPlay g target
    pushAll
      [Discard (toTarget att) | att <- toList g.entities.attachments, att.placement == AttachedTo target]
    pure $ g' & entitiesL . alliesL %~ Map.delete aid & playersL . ix a.owner . handL %~ (<> [toCard a])
  ShuffleIntoOwnersDeck target@(AllyTarget aid) | Just a <- Map.lookup aid g.entities.allies -> do
    g' <- leftPlay g target
    pushAll
      [Discard (toTarget att) | att <- toList g.entities.attachments, att.placement == AttachedTo target]
    deck <- shuffleM . (toCard a :) . (.deck) =<< getPlayer a.owner
    pure $ g' & entitiesL . alliesL %~ Map.delete aid & playersL . ix a.owner . deckL .~ deck
  PutOnBottomOfOwnersDeck target@(AllyTarget aid) | Just a <- Map.lookup aid g.entities.allies -> do
    g' <- leftPlay g target
    pushAll
      [Discard (toTarget att) | att <- toList g.entities.attachments, att.placement == AttachedTo target]
    pure $ g' & entitiesL . alliesL %~ Map.delete aid & playersL . ix a.owner . deckL %~ (<> [toCard a])
  -- an attachment returned to its owner's hand, as Bard son of Brand and Bartering return them
  ReturnToHand (AttachmentTarget aid) | Just a <- Map.lookup aid g.entities.attachments -> do
    let owner = (toAttrs a).owner
    pure $ g & entitiesL . attachmentsL %~ Map.delete aid & playersL . ix owner . handL %~ (<> [toCard a])
  ShuffleIntoOwnersDeck (AttachmentTarget aid) | Just a <- Map.lookup aid g.entities.attachments -> do
    let owner = (toAttrs a).owner
    deck <- shuffleM . (toCard a :) . (.deck) =<< getPlayer owner
    pure $ g & entitiesL . attachmentsL %~ Map.delete aid & playersL . ix owner . deckL .~ deck
  -- just before the attack's After windows, while what lasted for it still does
  AttackDefended _ eid defenders -> do
    undamaged <- filterM (fmap (notElem DamagedWhileDefending) . getModifiers) defenders
    readying <- elem ReadyUndamagedDefenders <$> getModifiers GameTarget
    pushAll [Ready d | readying, d <- undamaged, inPlay g d]
    pushWhen (notNull undamaged) $ CheckWindows [mkAfter $ DefendedWithoutDamage d eid | d <- undamaged]
    pure g
  GiveFromHand giver cid receiver -> do
    let given = [c | c <- maybe [] (.hand) (Map.lookup giver g.players), c.id == cid]
    pure $ g & playersL . ix giver . handL %~ filter ((/= cid) . (.id)) & playersL . ix receiver . handL %~ (<> given)
  ShuffleFromHandIntoOwnersDeck holder cid -> do
    let held = [c | c <- maybe [] (.hand) (Map.lookup holder g.players), c.id == cid]
        g' = g & playersL . ix holder . handL %~ filter ((/= cid) . (.id))
    foldM
      ( \acc c -> case c of
          PlayerCard pc | Just owner <- pc.owner -> do
            deck <- shuffleM (c : maybe [] (.deck) (Map.lookup owner acc.players))
            pure $ acc & playersL . ix owner . deckL .~ deck
          -- an encounter card held in hand, as Lost in the Wild, goes into the holder's
          -- deck: nothing keeps an encounter card out of a player's deck, as Recurring
          -- Nightmare is shuffled into one
          EncounterCard _ -> do
            deck <- shuffleM (c : maybe [] (.deck) (Map.lookup holder acc.players))
            pure $ acc & playersL . ix holder . deckL .~ deck
          _ -> pure acc
      )
      g'
      held
  -- a hero shuffled into its owner's deck, as Lost and Alone has it, is put into play as it is drawn
  ShuffleIntoOwnersDeck target@(HeroTarget hid) | Just h <- Map.lookup hid g.entities.heroes -> do
    g' <- leftPlay g target
    pushAll
      $ [Discard (toTarget att) | att <- toList g.entities.attachments, att.placement == AttachedTo target]
      <> [Discard (toTarget t) | t <- toList g.entities.treacheries, t.placement == AttachedTo target]
      <> [Discard (toTarget o) | o <- toList g.entities.objectives, o.placement == AttachedTo target]
    deck <- shuffleM . (toCard h :) . (.deck) =<< getPlayer h.owner
    pure $ g' & entitiesL . heroesL %~ Map.delete hid & playersL . ix h.owner . deckL .~ deck
  ReturnToTopOfEncounterDeck target -> returnToEncounterDeck (:) target g
  ReturnToBottomOfEncounterDeck target -> returnToEncounterDeck (\card deck -> deck <> [card]) target g
  RemoveProgressFromQuest source n -> do
    current <- getCurrentQuest
    let quests = [q | q <- toList g.entities.quests, Just (toTarget q) == current]
    pushAll [RemoveTokens source (toTarget q) Progress (min n q.progress) | q <- quests, q.progress > 0]
    pure g
  PutShadowEnemyIntoPlay host cid pid -> do
    for_ (Map.lookup host g.entities.enemies) \hostEnemy ->
      for_ (find ((== cid) . (.id)) hostEnemy.shadowCards) \card -> do
        enemy <- newEnemy card
        pushAll [RemoveShadowCard host cid, EngageEnemy pid enemy.id]
        putGame $ g & entitiesL . enemiesL %~ insertEntity enemy
    getGame
  PutShadowEnemyIntoStaging host cid -> do
    for_ (Map.lookup host g.entities.enemies) \hostEnemy ->
      for_ (find ((== cid) . (.id)) hostEnemy.shadowCards) \card -> do
        enemy <- newEnemy card
        pushAll [RemoveShadowCard host cid, CheckWindows [mkAfter $ EntersStagingArea (toTarget enemy)]]
        putGame $ g & entitiesL . enemiesL %~ insertEntity (overAttrs (\a -> a {enemyPlacement = StagingArea}) enemy)
    getGame
  PutShadowCardIntoPlay host cid pid -> do
    for_ (Map.lookup host g.entities.enemies) \hostEnemy ->
      for_ (find ((== cid) . (.id)) hostEnemy.shadowCards) \card -> do
        attacker <- newEnemy card
        pushAll
          [ RemoveShadowCard host cid
          , -- it never engages the player (Wolf Rider rulings)
            PlaceEnemy attacker.id (AttackingFromShadow pid)
          , CreateEffect
              $ Effect
                (toSource attacker)
                (toTarget attacker)
                [ReturnToTopOfEncounterDeckAfterCombat]
                UntilEndOfRound
          , DealShadowCard attacker.id
          , EnemyAttack pid attacker.id
          ]
        putGame $ g & entitiesL . enemiesL %~ insertEntity attacker
    getGame
  EngageOrcGuard pid card -> do
    guard' <- newEnemy (MkEncounterCard card.id (toCardCode orcGuard))
    push $ CheckWindows [mkAfter $ EnemyEngaged guard'.id pid]
    pure
      $ g
      & entitiesL
      . enemiesL
      %~ insertEntity (overAttrs (\a -> a {enemyPlacement = EngagedWith pid}) guard')
      & scenarioL
      %~ overAttrs (orcGuardsL %~ Map.insert card.id card)
  -- a reanimated card leaves its owner's deck, hand or discard pile for the staging area,
  -- a Reanimated Dead; leaving play, it goes to its owner's discard pile as an Orc Guard does
  Reanimate cid eid
    | Just (owner, pc) <- headMay [(p.id, c) | p <- toList g.players, PlayerCard c <- p.deck <> p.hand <> p.discard, c.id == cid] -> do
        let dead = createEnemy (MkEncounterCard cid (toCardCode reanimatedDead)) eid
            without = filter ((/= cid) . (.id))
        push $ CheckWindows [mkAfter $ EntersStagingArea (toTarget eid)]
        pure
          $ g
          & playersL
          . ix owner
          %~ ((deckL %~ without) . (handL %~ without) . (discardL %~ without))
          & entitiesL
          . enemiesL
          %~ insertEntity (overAttrs (\a -> a {enemyPlacement = StagingArea}) dead)
          & scenarioL
          %~ overAttrs (orcGuardsL %~ Map.insert cid pc)
  -- an enemy put into play engaged enters play, as Haradrim Elite reads
  PutEnemyIntoPlayEngaged pid card -> do
    enemy <- newEnemy card
    pushAll [EngageEnemy pid enemy.id, CheckWindows [mkAfter $ EntersPlay $ toTarget enemy]]
    pure
      $ g
      & entitiesL
      . enemiesL
      %~ insertEntity (overAttrs (\a -> a {enemyPlacement = StagingArea}) enemy)
  -- taken from the encounter discard pile, if it lies there, as From the Depths discards it
  GrappleCard card -> do
    enemy <- newEnemy card
    push $ ResolveGrapple enemy.id
    pure $ g & entitiesL . enemiesL %~ insertEntity enemy & scenarioL %~ overAttrs (encounterDiscardL %~ filter (/= card))
  ResolveGrapple eid | Just e <- Map.lookup eid g.entities.enemies -> do
    lead <- getLead
    let inPlay' l = l.placement `elem` [StagingArea, ActiveLocation]
        locations = filter inPlay' (toList g.entities.locations)
        picked = case [w | Grapple w <- toList (cdKeywords $ toCardDef e)] of
          w : _ -> do
            let grappling l = length [x | x <- toList g.entities.enemies, x.placement == AttachedTo (LocationTarget $ toId l)]
                value l
                  | w `elem` [MostEnemiesGrappled, FewestEnemiesGrappled] = grappling l
                  | otherwise = fromMaybe 0 $ printedFor w (toCardDef l)
                best = (if lowest w then minimumMay else maximumMay) (map value locations)
            [toId l | l <- locations, Just (value l) == best]
          _ -> []
        grapple lid = PlaceEnemy eid (AttachedTo $ LocationTarget lid)
    case picked of
      [] -> pushWhen (e.placement == Unplaced) $ PlaceEnemy eid StagingArea
      [lid] -> push $ grapple lid
      tied -> push $ questionLabelWith (ikey "label.grapple") (Just $ toTarget eid) [] lead [targetLabel lid [grapple lid] | lid <- tied]
    pure g
  CreateEnemy eid card ->
    pure $ g & entitiesL . enemiesL %~ insertEntity (overAttrs (\a -> a {enemyPlacement = StagingArea}) (createEnemy card eid))
  CreateEnemyEngaged pid eid card -> do
    pushAll [EngageEnemy pid eid, CheckWindows [mkAfter $ EntersPlay $ EnemyTarget eid]]
    pure $ g & entitiesL . enemiesL %~ insertEntity (overAttrs (\a -> a {enemyPlacement = StagingArea}) (createEnemy card eid))
  PutIntoPlayAsActiveLocation card -> do
    thisLocation <- newLocation card
    pure $ g & entitiesL . locationsL %~ insertEntity (overAttrs (\a -> a {locationPlacement = ActiveLocation}) thisLocation)
  PutLocationIntoPlay card placement -> do
    thisLocation <- newLocation card
    pure $ g & entitiesL . locationsL %~ insertEntity (overAttrs (\a -> a {locationPlacement = placement}) thisLocation)
  PutObjectiveIntoPlayGuardedBy card cid -> do
    thisObjective <- newObjective card
    pushAll [SetObjectiveGuard (toId thisObjective) cid, CheckWindows [mkAfter $ EntersStagingArea (toTarget thisObjective)]]
    pure $ g & entitiesL . objectivesL %~ insertEntity (overAttrs (\a -> a {objectivePlacement = StagingArea}) thisObjective)
  -- a Sack card drawn comes into play, unattached, to resolve its When Sacked effect
  DrawSackCard card -> do
    sack <- newObjective card
    push $ WhenSacked (toId sack)
    pure $ g & entitiesL . objectivesL %~ insertEntity sack
  ReturnToSackDeck oid | Just o <- Map.lookup oid g.entities.objectives -> do
    push $ ShuffleIntoSackDeck (MkEncounterCard (objectiveCardId $ toAttrs o) (toCardCode o))
    pure $ g & entitiesL . objectivesL %~ Map.delete oid
  PutObjectiveIntoPlayAttached card host -> do
    thisObjective <- newObjective card
    pure $ g & entitiesL . objectivesL %~ insertEntity (overAttrs (\a -> a {objectivePlacement = AttachedTo host}) thisObjective)
  RemoveFromGame (ObjectiveTarget oid) -> pure $ g & entitiesL . objectivesL %~ Map.delete oid
  RemoveFromGame (ContractTarget cid) -> pure $ g & entitiesL . contractsL %~ Map.delete cid
  -- a treachery attached as a condition leaves play for good, as Daechanar removes a Sorcery card
  RemoveFromGame (TreacheryTarget tid) -> pure $ g & entitiesL . treacheriesL %~ Map.delete tid
  -- an enemy leaves play for good, as Change of Plans removes the Corsair Raiders
  -- an enemy leaves play for good, as Captain Sahír after his betrayal: what is on it is
  -- discarded
  RemoveFromGame target@(EnemyTarget eid) | Map.member eid g.entities.enemies -> do
    pushAll $ discardAttached g target
    pure $ g & entitiesL . enemiesL %~ Map.delete eid
  -- a side quest removed from the game, as Stop the Executioners! removes itself
  RemoveFromGame target@(QuestTarget qid) | Just q <- Map.lookup qid g.entities.quests, isSideQuest q -> do
    pushAll $ discardAttached g target
    pure $ g & entitiesL . questsL %~ Map.delete qid
  -- a hero leaves play for good, not for its owner's discard pile, its attachments
  -- discarded as they would be were it discarded
  SetAsideHero hid -> runGameMessage (RemoveFromGame $ HeroTarget hid) g
  -- an enemy set aside leaves play as one removed from the game would, its card kept aside
  SetAsideEnemy eid | Just e <- Map.lookup eid g.entities.enemies -> do
    g' <- runGameMessage (RemoveFromGame $ EnemyTarget eid) g
    pure $ g' & scenarioL %~ overAttrs (setAsideL %~ (MkEncounterCard (enemyCardId $ toAttrs e) (toCardCode e) :))
  SetAsideObjective oid | Just o <- Map.lookup oid g.entities.objectives -> do
    g' <- runGameMessage (RemoveFromGame $ ObjectiveTarget oid) g
    pure $ g' & scenarioL %~ overAttrs (setAsideL %~ (MkEncounterCard (objectiveCardId $ toAttrs o) (toCardCode o) :))
  RemoveFromGame target@(HeroTarget hid) | Map.member hid g.entities.heroes -> do
    g' <- leftPlay g target
    pushAll $ discardAttached g target
    pure $ g' & entitiesL . heroesL %~ Map.delete hid
  -- an ally leaves play for good, as Eagle of the North does however it leaves
  RemoveFromGame target@(AllyTarget aid) | Map.member aid g.entities.allies -> do
    g' <- leftPlay g target
    pushAll $ discardAttached g target
    pure $ g' & entitiesL . alliesL %~ Map.delete aid
  -- what is attached to it leaves play with it, as it would were it discarded
  RemoveFromGame target@(LocationTarget lid) -> do
    pushAll $ discardAttached g target
    pure $ g & entitiesL . locationsL %~ Map.delete lid
  -- a location placed underneath a card leaves play, its tokens and attachments discarded;
  -- so does a character or an attachment captured from play (Escape from Mount Gram), and
  -- a treachery placed there as it resolves, as Closing In
  PlaceUnderneath _ cards -> do
    let under :: IsCard a => a -> Bool
        under c = (toCard c).id `elem` map (.id) cards
        gone = filter under (toList g.entities.locations)
        heroes = filter under (toList g.entities.heroes)
        allies = filter under (toList g.entities.allies)
        enemies = filter under (toList g.entities.enemies)
        characters = map toTarget heroes <> map toTarget allies
        on' :: HasField "placement" a Placement => a -> Bool
        on' a = a.placement `elem` map (AttachedTo . toTarget) gone
    g' <- foldM leftPlay g characters
    pushAll
      $ [Discard (toTarget a) | a <- toList g.entities.attachments, on' a]
      <> [Discard (toTarget t) | t <- toList g.entities.treacheries, on' t]
      <> [Discard (toTarget o) | o <- toList g.entities.objectives, on' o]
      <> concatMap (discardAttached g) characters
      <> [CheckEliminations | notNull heroes]
      -- an enemy placed underneath a location leaves play, as Murderous Turncoat
      <> concatMap (discardAttached g . toTarget) enemies
    pure
      $ g'
      & entitiesL . locationsL %~ (\ls -> foldr (Map.delete . toId) ls gone)
      & entitiesL . enemiesL %~ Map.filter (not . under)
      & entitiesL . heroesL %~ Map.filter (not . under)
      & entitiesL . alliesL %~ Map.filter (not . under)
      & entitiesL . attachmentsL %~ Map.filter (not . under)
      & entitiesL . treacheriesL %~ Map.filter (not . under)
  PutObjectiveIntoPlay card -> do
    thisObjective <- newObjective card
    let guarded = \case
          Guarded -> True
          GuardedBy _ -> True
          _ -> False
    pushWhen (any guarded $ cdKeywords (toCardDef card)) $ GuardObjective (toId thisObjective)
    push $ CheckWindows [mkAfter $ EntersStagingArea (toTarget thisObjective)]
    -- an objective's When Revealed, as Durin's Greaves attaching itself, after the window
    -- that can cancel it, as an enemy's, and the window after it, as "after Frodo's Choice
    -- is revealed" reads
    when (cdWhenRevealed (toCardDef card))
      $ pushAll [CheckWindows [mkWhen $ Revealed (toTarget thisObjective)], WhenRevealed (toTarget thisObjective), CheckWindows [mkAfter $ Revealed (toTarget thisObjective)]]
    pure
      $ g
      & entitiesL
      . objectivesL
      %~ insertEntity (overAttrs (\a -> a {objectivePlacement = StagingArea}) thisObjective)
  TakeControl aid pid -> do
    push $ CheckWindows [mkAfter $ ChangedControl (AllyTarget aid)]
    pure $ g & entitiesL . alliesL . ix aid %~ overAttrs (\a -> a {allyPlacement = InPlayArea pid})
  -- a side quest defeated goes to the victory display, and the quest stays where it is
  -- a side quest is chosen for the phase; a main quest while several are in play, as The
  -- Battle of Five Armies' stages, for the round (The Hobbit: On the Doorstep rules insert,
  -- "Current Quest Stage"), a side quest chosen later in the round only putting it aside
  -- for the phase
  SetCurrentQuest t -> do
    sideQuests <- getSideQuests
    mainQuests <- getMainQuests
    let side = t `elem` sideQuests
        chosen =
          [Effect GameSource t [CurrentQuest] UntilEndOfPhase | side]
            <> [Effect GameSource t [CurrentQuest] UntilEndOfRound | t `elem` mainQuests, length mainQuests > 1]
        replaced e = CurrentQuest `elem` effectModifiers e && (not side || effectTarget e `elem` sideQuests)
    pure $ g & effectsL %~ ((chosen <>) . filter (not . replaced))
  CompleteQuest qid | Just q <- Map.lookup qid g.entities.quests, isSideQuest q -> do
    pushAll $ CheckWindows [mkAfter $ QuestDefeated (QuestTarget qid)] : discardAttachedToQuest g qid
    -- "Limit 1 copy in the victory display": a copy defeated while one is there goes to its
    -- owner's discard pile (The Sands of Harad rules)
    let shown = any ((== toCardCode q) . toCardCode) (toAttrs g.scenario).victoryDisplay
    case toCard q of
      PlayerCard pc | shown && cdLimitOneInVictoryDisplay (toCardDef q) -> do
        push $ PutInDiscard pc
        pure $ g & entitiesL . questsL %~ Map.delete qid
      card -> pure $ g & entitiesL . questsL %~ Map.delete qid & scenarioL %~ overAttrs (victoryDisplayL %~ (card :))
  CompleteQuest qid -> do
    pushAll $ CheckWindows [mkAfter $ QuestDefeated (QuestTarget qid)] : discardAttachedToQuest g qid <> [AdvanceQuest]
    -- a stage with victory points goes to the victory display, as Search for an Exit's
    let won = [toCard q | q <- toList (Map.lookup qid g.entities.quests), isJust (cdVictoryPoints $ toCardDef q)]
    pure $ g & entitiesL . questsL %~ Map.delete qid & scenarioL %~ overAttrs (victoryDisplayL %~ (won <>))
  LeaveQuestStage qid | Map.member qid g.entities.quests -> do
    pushAll $ discardAttachedToQuest g qid
    pure $ g & entitiesL . questsL %~ Map.delete qid
  -- a stage added to the victory display leaves play; what comes next is the card's to say
  AddToVictoryDisplay (QuestTarget qid) | Just q <- Map.lookup qid g.entities.quests ->
    pure $ g & entitiesL . questsL %~ Map.delete qid & scenarioL %~ overAttrs (victoryDisplayL %~ (toCard q :))
  ShuffleIntoQuestDeck card -> pure $ g & entitiesL . questsL %~ Map.filter ((/= card.id) . (.cardId) . toAttrs)
  PutOnBottomOfQuestDeck card -> pure $ g & entitiesL . questsL %~ Map.filter ((/= card.id) . (.cardId) . toAttrs)
  AddToVictoryDisplay target@(LocationTarget lid) | Just l <- Map.lookup lid g.entities.locations -> do
    pushAll
      $ [Discard (toTarget a) | a <- toList g.entities.attachments, a.placement == AttachedTo target]
      <> [Discard (toTarget t) | t <- toList g.entities.treacheries, t.placement == AttachedTo target]
      -- an objective it guarded, as Mugash on Edge of Fangorn, goes free to the staging area
      <> [Discard (toTarget o) | o <- toList g.entities.objectives, o.placement == AttachedTo target]
      <> freeGuarded g target
    pure
      $ g
      & entitiesL
      . locationsL
      %~ Map.delete lid
      & scenarioL
      %~ overAttrs
        (victoryDisplayL %~ (EncounterCard (MkEncounterCard (locationCardId $ toAttrs l) (toCardCode l)) :))
  -- a card from the encounter deck, or a player's discard pile, as Out of the Wild takes them
  AddToVictoryDisplay (CardIdTarget cid) -> do
    let fromDeck = [EncounterCard c | c <- (toAttrs g.scenario).encounterDeck, c.id == cid]
        -- a destroyed enemy or an explored location, as None Return and Leave No Trace add
        fromEncounterDiscard = [EncounterCard c | c <- (toAttrs g.scenario).encounterDiscard, c.id == cid]
        fromDiscards = [c | p <- toList g.players, c <- p.discard, c.id == cid]
    pure
      $ g
      & scenarioL
      %~ overAttrs
        ( (encounterDeckL %~ filter ((/= cid) . (.id)))
            . (encounterDiscardL %~ filter ((/= cid) . (.id)))
            . (victoryDisplayL %~ ((fromDeck <> fromEncounterDiscard <> fromDiscards) <>))
        )
      & playersL
      . traverse
      . discardL
      %~ filter ((/= cid) . (.id))
  -- an enemy added to the victory display leaves play, what was attached to it and its
  -- shadow cards discarded
  AddToVictoryDisplay target@(EnemyTarget eid) | Just e <- Map.lookup eid g.entities.enemies -> do
    pushAll
      $ [Discard (toTarget a) | a <- toList g.entities.attachments, a.placement == AttachedTo target]
      <> [Discard (toTarget t) | t <- toList g.entities.treacheries, t.placement == AttachedTo target]
      <> attachedEnemies g target
      <> [AddToEncounterDiscard e.shadowCards | notNull e.shadowCards]
      <> freeGuarded g target
    pure
      $ g
      & entitiesL
      . enemiesL
      %~ Map.delete eid
      & scenarioL
      %~ overAttrs (victoryDisplayL %~ (EncounterCard (MkEncounterCard (enemyCardId $ toAttrs e) (toCardCode e)) :))
  AddToVictoryDisplay (AttachmentTarget aid)
    | Just a <- Map.lookup aid g.entities.attachments ->
        pure
          $ g
          & entitiesL
          . attachmentsL
          %~ Map.delete aid
          & scenarioL
          %~ overAttrs (victoryDisplayL %~ (toCard a :))
  -- a treachery adding itself as it resolves, as An Evil Fortune does
  AddToVictoryDisplay (TreacheryTarget tid)
    | Just t <- Map.lookup tid g.entities.treacheries ->
        pure $ g & entitiesL . treacheriesL %~ Map.delete tid & scenarioL %~ overAttrs (victoryDisplayL %~ (toCard t :))
  -- an objective claimed into the victory display, as a Clue of The Steward's Fear
  AddToVictoryDisplay (ObjectiveTarget oid)
    | Just o <- Map.lookup oid g.entities.objectives ->
        pure $ g & entitiesL . objectivesL %~ Map.delete oid & scenarioL %~ overAttrs (victoryDisplayL %~ (toCard o :))
  MakeEscapeTest t -> do
    order <- getPlayerOrder
    own <- charactersControlledBy t.escapePlayer
    limits <- (\ms -> [k | EscapeTestCommitLimit k <- ms]) <$> getModifiers t.escapePlayer
    -- one with a limit, as Ill-fated Guard sets, commits no more once he has reached it
    let full = any (<= length (filter (`elem` own) t.escapeCommitted)) limits
    ready <-
      if full
        then pure []
        else
          filter (`notElem` t.escapeCommitted)
            <$> selectCharacters
              (HeroControlledBy (PlayerWithId t.escapePlayer) <> ReadyHero)
              (AllyControlledBy (PlayerWithId t.escapePlayer) <> ReadyAlly)
    push
      $ questionLabelWith
        (ikey "label.escapeTest")
        (sourceTarget t.escapeSource)
        t.escapeCommitted
        t.escapePlayer
      $ [ targetLabel c [Exhaust c, CheckWindows [mkAfter $ CommittedToEscapeTest c], MakeEscapeTest t {escapeCommitted = t.escapeCommitted <> [c]}]
        | c <- ready
        ]
      <> [ Label (ikey "label.doneCommittingToEscape") $ case t.escapeParty of
             next : rest -> [MakeEscapeTest t {escapePlayer = next, escapeParty = rest}]
             -- the players may act once the characters are committed, before the cards
             -- are dealt, the test's only action window
             [] -> map OptionalActionWindow order <> [ResolveEscapeTest t]
         ]
    pure g
  ExhaustToReduceCost pid costId trait -> do
    healers <-
      exhaustableCharacters
        (HeroControlledBy (PlayerWithId pid) <> HeroWithTrait trait)
        (AllyControlledBy (PlayerWithId pid) <> AllyWithTrait trait)
    let owed = maybe 0 (staticCost . (.activeCostCost)) (Map.lookup costId (gameActiveCosts g))
    push
      $ if null healers || owed <= 0
        then PayCost costId
        else
          questionLabel (ikeyWith "label.exhaustToReduceCost" [intVar "count" owed]) pid
            $ [ targetLabel c [Exhaust c, ReduceActiveCost costId 1, ExhaustToReduceCost pid costId trait]
              | c <- healers
              ]
            <> [Label (ikey "label.payRemainingCost") [PayCost costId]]
    pure g
  -- a Hide test: the player may exhaust any number of characters he controls to commit
  -- them, then the cards are discarded (The Black Riders rules insert, "Hide Tests")
  MakeHideTest t -> do
    ready <-
      filter (`notElem` t.hideCommitted)
        <$> selectCharacters
          (HeroControlledBy (PlayerWithId t.hidePlayer) <> ReadyHero)
          (AllyControlledBy (PlayerWithId t.hidePlayer) <> ReadyAlly)
    push
      $ questionLabelWith (ikey "label.hideTest") (sourceTarget t.hideSource) t.hideCommitted t.hidePlayer
      $ [targetLabel c [Exhaust c, MakeHideTest t {hideCommitted = t.hideCommitted <> [c]}] | c <- ready]
      <> [Label (ikey "label.doneCommittingToHide") [DiscardForHideTest t]]
    pure g
  -- passed if the committed willpower is at least the discarded cards' threat; after it,
  -- the characters are no longer committed to it
  ResolveHideTest t -> do
    (willpower, threat) <- hideTestTotals t
    pushAll
      $ if threat > willpower
        then [FailHideTest t.hidePlayer, CheckWindows [mkAfter $ FailedHideTest t.hidePlayer]]
        else [CheckWindows [mkAfter $ PassedHideTest t.hidePlayer]]
    pure g
  FailHideTest pid -> do
    pushAll
      [ CreateEffect $ Effect GameSource (toTarget pid) [FailedHideTestThisPhase] UntilEndOfPhase
      , CreateEffect $ Effect GameSource (toTarget pid) [FailedHideTestThisRound] UntilEndOfRound
      ]
    pure g
  ResolveEscapeTest t -> do
    (total, escape) <- escapeTestTotals t
    let (dealt, rest) = splitAt t.escapeCards (toAttrs g.scenario).encounterDeck
    pushAll $ if escape > total then t.escapeOnFail else t.escapeOnPass
    pure $ g & scenarioL %~ overAttrs ((encounterDeckL .~ rest) . (encounterDiscardL %~ (dealt <>)))
  -- he may exhaust any number of heroes he controls to commit them to the test (The
  -- Mountain of Fire rules insert, "Fortitude Tests")
  MakeFortitudeTest t -> do
    heroes <- select (HeroControlledBy (PlayerWithId t.fortitudePlayer) <> ReadyHero)
    ready <- filterM (fmap (notElem CannotCommitToFortitudeTests) . getModifiers . HeroTarget) (filter (`notElem` t.fortitudeCommitted) heroes)
    push
      $ questionLabelWith
        (ikey "label.fortitudeTest")
        (sourceTarget t.fortitudeSource)
        (map HeroTarget t.fortitudeCommitted)
        t.fortitudePlayer
      $ [ targetLabel (HeroTarget h) [Exhaust (HeroTarget h), MakeFortitudeTest t {fortitudeCommitted = t.fortitudeCommitted <> [h]}]
        | h <- ready
        ]
      <> [Label (ikey "label.doneCommittingToFortitudeTest") [DiscardForFortitudeTest t]]
    pure g
  -- he discards the top X cards of the encounter deck, X the tens digit of his threat,
  -- the discard pile shuffled back in should the deck run out; the players may then act,
  -- the test's action window, once the Eye icons are known
  DiscardForFortitudeTest t -> do
    threat <- getThreat t.fortitudePlayer
    let s = toAttrs g.scenario
        x = threat `div` 10 + t.fortitudeExtraCards
        (top, rest) = splitAt x s.encounterDeck
    (discarded, deck, pile) <-
      if length top < x && notNull s.encounterDiscard
        then do
          shuffled <- shuffleM s.encounterDiscard
          let (more, rest') = splitAt (x - length top) shuffled
          pure (top <> more, rest', [])
        else pure (top, rest, s.encounterDiscard)
    order <- getPlayerOrder
    pushAll
      $ [CheckWindows [mkAfter $ DiscardedForFortitudeTest t.fortitudePlayer discarded] | notNull discarded]
      <> map OptionalActionWindow order
      <> [ResolveFortitudeTest]
    pure
      $ g
      & fortitudeTestsL %~ (t {fortitudeDiscarded = discarded} :)
      & scenarioL %~ overAttrs (\a -> a & encounterDeckL .~ deck & encounterDiscardL .~ (reverse discarded <> pile))
  IgnoreFortitudeEyeIcons cid -> pure $ g & fortitudeTestsL %~ \case
    t : ts -> t {fortitudeIgnored = t.fortitudeIgnored <> [cid]} : ts
    [] -> []
  -- passed if the committed heroes' willpower is greater than the Eye icons; ties fail
  ResolveFortitudeTest -> case gameFortitudeTests g of
    t : ts -> do
      (willpower, eyes) <- fortitudeTestTotals t
      let pid = t.fortitudePlayer
      pushAll
        $ if willpower > eyes
          then t.fortitudeOnPass
          else
            CreateEffect (Effect t.fortitudeSource (PlayerTarget pid) [FailedFortitudeTestThisRound] UntilEndOfRound)
              : t.fortitudeOnFail
              <> [CheckWindows [mkAfter $ FailedFortitudeTest pid]]
      pure $ g & fortitudeTestsL .~ ts
    [] -> pure g
  -- a shadow card that is a location becomes the active location
  PutShadowLocationIntoPlay host cid -> do
    for_ (Map.lookup host g.entities.enemies) \hostEnemy ->
      for_ (find ((== cid) . (.id)) hostEnemy.shadowCards) \card -> do
        thisLocation <- newLocation card
        active <- select IsActiveLocation
        pushAll $ RemoveShadowCard host cid : [PlaceLocation l StagingArea | l <- active]
        putGame
          $ g
          & entitiesL
          . locationsL
          %~ insertEntity (overAttrs (\a -> a {locationPlacement = ActiveLocation}) thisLocation)
    getGame
  ResetToStage card _ -> do
    pushAll [PutQuestIntoPlay card, Flip (toTarget card)]
    pure $ g & entitiesL . questsL %~ Map.filter isSideQuest
  -- a Guarded (X) attachment is guarded before it is attached (MEC65 rules)
  PlayAttachment pid card | Just types <- guardedTypes card -> g <$ push (ResolveGuardedPlayerCard pid card types)
  PlayAttachment pid card -> playAttachment pid card g
  -- free of encounters, it attaches to an eligible card, or with none is discarded
  -- the card is claimed once attached, as an Ered Mithrin treasure's "After you claim"
  AttachFreedCard pid card -> do
    hosts <- cardHosts pid (toCardCode card)
    if null hosts
      then g <$ push (PutInDiscard card)
      else do
        push $ CheckWindows [mkAfter $ Claimed (CardIdTarget card.id)]
        playAttachment pid card g
  -- a character can have at most 2 restricted attachments, claimed objectives among
  -- them: with a third, its controller discards one of them
  AttachCard aid host
    | Just a <- Map.lookup aid g.entities.attachments
    , isRestricted a ->
        g <$ checkRestricted g host
  ClaimObjective oid host
    | Just o <- Map.lookup oid g.entities.objectives
    , isRestricted o ->
        g <$ checkRestricted g host
  CheckRestricted host -> g <$ checkRestricted g host
  PlayAttachmentOn pid card host -> do
    attachment <- newAttachment pid card
    pushAll
      [AttachCard (toId attachment) host, CheckWindows [mkAfter $ EntersPlay $ toTarget attachment]]
    pure $ g & entitiesL . attachmentsL %~ insertEntity attachment
  SwapHero hid card | Just old <- Map.lookup hid g.entities.heroes -> do
    -- it trades places with the old hero, ready or exhausted as that one was (ruling)
    new <- overAttrs (\a -> a {heroExhausted = heroExhausted (toAttrs old)}) <$> newHero old.owner card
    let on' :: HasField "placement" a Placement => a -> Bool
        on' a = a.placement == AttachedTo (toTarget old)
        newHost = AttachedTo (toTarget new)
    putGame
      $ g
      & entitiesL
      . heroesL
      %~ (insertEntity new . Map.delete hid)
      -- the encounter's conditions and claimed objectives go with it
      & entitiesL
      . treacheriesL
      . traverse
      %~ (\t -> if on' t then overAttrs (\a -> a {treacheryPlacement = newHost}) t else t)
      & entitiesL
      . objectivesL
      . traverse
      %~ (\o -> if on' o then overAttrs (\a -> a {objectivePlacement = newHost}) o else o)
    moves <- for (filter on' $ toList g.entities.attachments) \a -> do
      hosts <- attachmentHosts (toAttrs a).attachTo
      pure
        $ if toTarget new `elem` hosts then AttachCard (toId a) (toTarget new) else Discard (toTarget a)
    pushAll $ moves <> [CheckWindows [mkAfter $ EntersPlay $ toTarget new]]
    getGame
  -- the card keeps its id under the other card type, as its own text finds it by
  AllyBecomesHero aid | Just a <- Map.lookup aid g.entities.allies -> do
    let hid = HeroId (unAllyId aid)
        attrs = toAttrs a
        controller = fromMaybe attrs.allyOwner (placementController a.placement)
        new =
          overAttrs
            ( \h ->
                h
                  { heroTokens = allyTokens attrs
                  , heroExhausted = allyExhausted attrs
                  , heroCommitted = allyCommitted attrs
                  , heroAttacking = allyAttacking attrs
                  , heroFacedown = allyFacedown attrs
                  , heroPoison = allyPoison attrs
                  , heroUnconscious = allyUnconscious attrs
                  }
            )
            $ lookupHero (toCardCode a) attrs.allyCardId hid controller
    pure $ retargetCharacter (toTarget a) (toTarget new) $ g & entitiesL . alliesL %~ Map.delete aid & entitiesL . heroesL %~ insertEntity new
  HeroBecomesAlly hid | Just h <- Map.lookup hid g.entities.heroes, cdCardType (toCardDef h) == AllyType -> do
    let aid = AllyId (unHeroId hid)
        attrs = toAttrs h
        new =
          overAttrs
            ( \a ->
                a
                  { allyPlacement = InPlayArea attrs.heroOwner
                  , allyTokens = heroTokens attrs
                  , allyExhausted = heroExhausted attrs
                  , allyCommitted = heroCommitted attrs
                  , allyAttacking = heroAttacking attrs
                  , allyFacedown = heroFacedown attrs
                  , allyPoison = heroPoison attrs
                  , allyUnconscious = heroUnconscious attrs
                  }
            )
            $ lookupAlly (toCardCode h) attrs.heroCardId aid attrs.heroOwner
    pure $ retargetCharacter (toTarget h) (toTarget new) $ g & entitiesL . heroesL %~ Map.delete hid & entitiesL . alliesL %~ insertEntity new
  -- it does not leave play: the same card under the enemy card type, its owner's still, its
  -- damage kept. What was attached to it as an ally, a character, is discarded, and it
  -- engages its owner (Secret Cultist).
  AllyBecomesEnemy aid | Just a <- Map.lookup aid g.entities.allies -> do
    let attrs = toAttrs a
        eid = EnemyId (unAllyId aid)
        new =
          overAttrs
            (\e -> e {enemyTokens = allyTokens attrs, enemyFacedown = allyFacedown attrs, enemyOwner = Just attrs.allyOwner})
            $ lookupEnemy (toCardCode a) attrs.allyCardId eid
    pushAll $ discardAttached g (toTarget a) <> [EngageEnemy attrs.allyOwner eid]
    pure
      $ g
      & entitiesL
      . alliesL
      %~ Map.delete aid
      & entitiesL
      . enemiesL
      %~ insertEntity new
      & effectsL
      . traverse
      %~ (\e -> if effectTarget e == toTarget a then e {effectTarget = toTarget new} else e)
  -- it does not leave play: the same card under the enemy card type, its owner's still,
  -- its damage kept; what was attached to it as a hero is discarded (Fallen into Evil)
  HeroBecomesEnemy hid | Just h <- Map.lookup hid g.entities.heroes -> do
    let attrs = toAttrs h
        eid = EnemyId (unHeroId hid)
        new =
          overAttrs (\e -> e {enemyTokens = heroTokens attrs, enemyOwner = Just attrs.heroOwner})
            $ lookupEnemy (toCardCode h) attrs.heroCardId eid
    pushAll $ discardAttached g (toTarget h) <> [CheckEliminations]
    pure
      $ g
      & entitiesL . heroesL %~ Map.delete hid
      & entitiesL . enemiesL %~ insertEntity new
      & effectsL . traverse %~ (\e -> if effectTarget e == toTarget h then e {effectTarget = toTarget new} else e)
  -- back under its owner's control, its damage kept; what was attached to it as an enemy is
  -- discarded
  EnemyBecomesHero eid
    | Just e <- Map.lookup eid g.entities.enemies
    , Just owner <- enemyOwner (toAttrs e)
    , cdCardType (toCardDef e) == HeroType -> do
        let attrs = toAttrs e
            new =
              overAttrs (\h -> h {heroTokens = enemyTokens attrs})
                $ lookupHero (toCardCode e) attrs.enemyCardId (HeroId $ unEnemyId eid) owner
        pushAll $ discardAttached g (toTarget e)
        pure
          $ g
          & entitiesL . enemiesL %~ Map.delete eid
          & entitiesL . heroesL %~ insertEntity new
          & effectsL . traverse %~ (\x -> if effectTarget x == toTarget e then x {effectTarget = toTarget new} else x)
  -- it does not leave play: it keeps its id, and whether it is exhausted, as the card it is
  AllyBecomesAttachment aid host | Just a <- Map.lookup aid g.entities.allies -> do
    let attrs = toAttrs a
        new =
          overAttrs (\x -> x {attachmentExhausted = allyExhausted attrs})
            $ lookupAttachment (toCardCode a) attrs.allyCardId (AttachmentId $ unAllyId aid) attrs.allyOwner
    pushAll $ discardAttached g (toTarget a) <> [AttachCard (toId new) host, CheckRestricted host]
    pure $ g & entitiesL . alliesL %~ Map.delete aid & entitiesL . attachmentsL %~ insertEntity new
  PutQuestIntoPlay card -> do
    thisQuest <- newQuest card
    pure $ g & entitiesL . questsL %~ insertEntity thisQuest
  -- a card with the Peril keyword: the player who revealed it resolves its staging on his
  -- own, the other players taking no actions and triggering no responses until it has
  Do inner@(revealedCard -> Just _) -> runGameMessage inner g
  (revealedCard -> Just (engaging, card)) | Peril `member` cdKeywords (toCardDef card), isNothing (gamePeril g) -> do
    revealer <- maybe getLead pure (engaging <|> gameRevealer g)
    g <$ pushAll [SetPeril (Just revealer), Do msg, SetPeril Nothing]
  (revealedCard -> Just (_, card)) | Loot `member` cdKeywords (toCardDef card) -> g <$ push (DiscardLoot card)
  -- the next card revealed is discarded unresolved if it is of the type named, as Minas
  -- Tirith Lampwright reads; either way the effect is spent
  (revealedCard -> Just (_, card))
    | Just e <- find (any isDiscardNextRevealed . effectModifiers) (gameEffects g) -> do
        let g' = g & effectsL %~ filter (/= e)
        if cdCardType (toCardDef card) `elem` [t | DiscardNextRevealed t <- effectModifiers e]
          then g' <$ push (AddToEncounterDiscard [card])
          else runGameMessage msg g'
  (revealedCard -> Just (engaging, card)) -> do
    let def = toCardDef card
    players <- getPlayerOrder
    revealer <- maybe getLead pure (engaging <|> gameRevealer g)
    let doomed =
          concat
            [ [RaiseThreat p n, CheckWindows [mkAfter $ ThreatRaisedByEncounter p n, mkAfter $ ThreatRaisedByDoomed p]]
            | Doomed n <- toList (cdKeywords def)
            , n > 0
            , p <- players
            ]
        -- the players as a group discard the resources from their heroes' pools (The
        -- Drúadan Forest rules)
        prowl = [DiscardGroupResources lead n | Prowl n <- toList (cdKeywords def), n > 0, lead <- take 1 players]
        whenRevealed t = if cdWhenRevealed def then AskToResolveWhenRevealed t card else WhenRevealed t
        -- the first enemy revealed this phase takes the damage Expecting Mischief readies for
        -- it, before its When Revealed and keywords, as Thalin deals his (FAQ 1.9)
        mischief = take 1 [e | cdCardType def == EnemyType, e <- gameEffects g, any isDamageNextRevealedEnemy (effectModifiers e)]
        mischiefDamage t = [DealDamage (effectSource e) t n | e <- mischief, DamageNextRevealedEnemy n <- effectModifiers e]
        -- the player who revealed it makes the Hide test before the rest of the card
        -- resolves (The Black Riders rules insert, "Hide X")
        hide = [MakeHideTest $ HideTest revealer (CardIdSource card.id) n [] [] | Hide n <- toList (cdKeywords def)]
        -- Prowl, a keyword, resolves before the When Revealed effect (ALeP 3.094)
        revealing t =
          inLogBlock (RevealBlock card)
            $ [CheckWindows [mkAfter $ EntersStagingArea t] | cdCardType def `elem` [EnemyType, LocationType, EncounterSideQuestType], isNothing engaging]
            <> [CheckWindows [mkWhen $ Revealed t]] <> mischiefDamage t <> [WhileInPlay t prowl | notNull prowl] <> hide <> [whenRevealed t, WhileInPlay t doomed]
            <> [CheckWindows [mkAfter $ Revealed t]]
    case cdCardType def of
      -- an enemy revealed to be put into play engaged never enters the staging area
      EnemyType | Just pid <- engaging -> do
        enemy <- newEnemy card
        pushAll . ([EngageEnemy pid (toId enemy), CheckWindows [mkAfter $ EntersPlay $ toTarget enemy]] <>) . (<> [WhileInPlay (toTarget enemy) [ResolveSurge (toTarget enemy) card]])
          =<< revealing (toTarget enemy)
        pure $ g & effectsL %~ filter (`notElem` mischief) & entitiesL . enemiesL %~ insertEntity (overAttrs (\a -> a {enemyPlacement = Unplaced}) enemy)
      EnemyType -> do
        enemy <- newEnemy card
        let grapples = [ResolveGrapple (toId enemy) | any isGrapple (cdKeywords def)]
        pushAll . (<> grapples <> [WhileInPlay (toTarget enemy) [AmbushEngagementCheck (toId enemy), ResolveSurge (toTarget enemy) card]])
          =<< revealing (toTarget enemy)
        pure
          $ g
          & effectsL %~ filter (`notElem` mischief)
          & entitiesL
          . enemiesL
          %~ insertEntity (overAttrs (\a -> a {enemyPlacement = StagingArea}) enemy)
      LocationType -> do
        location <- newLocation card
        -- a guarded one, as Across the Ettenmoors' objective-locations, is guarded by the
        -- next card before its surge
        let guarded = guardLocation (toId location) def
        pushAll . (<> guarded <> [ResolveSurge (toTarget location) card]) =<< revealing (toTarget location)
        pure
          $ g
          & entitiesL
          . locationsL
          %~ insertEntity (overAttrs (\a -> a {locationPlacement = StagingArea}) location)
      TreacheryType -> do
        treachery <- newTreachery card
        -- counted as it is revealed, for "the first treachery card revealed each round"
        -- and for the phase, as "the first treachery revealed each quest phase"
        let counted = CreateEffect $ Effect (TreacherySource $ toId treachery) GameTarget [TreacheryRevealedThisRound] UntilEndOfRound
            countedForPhase = CreateEffect $ Effect (TreacherySource $ toId treachery) GameTarget [TreacheryRevealedThisPhase] UntilEndOfPhase
        pushAll . ([counted, countedForPhase] <>) . (<> [FinishRevelation (toId treachery), ResolveSurge (toTarget treachery) card])
          =<< revealing (toTarget treachery)
        pure $ g & entitiesL . treacheriesL %~ insertEntity treachery
      -- a player card with the Encounter keyword, as Ranger of the North: it enters play
      -- under the first player, its When Revealed effect giving it to a player
      AllyType | Just owner <- find (any ((== card.id) . (.id)) . (.setAsideCards)) (toList g.players) -> do
        lead <- getLead
        aid <- getRandom
        thisAlly <- entering $ overAttrs (\a -> a {allyPlacement = InPlayArea lead}) $ lookupAlly (toCardCode card) card.id aid owner.id
        -- it enters play as it is revealed, but whose it is is chosen as its When Revealed
        -- resolves, so the windows for it entering play open once that is done
        pushAll
          . (enteredPlayThisRound thisAlly :)
          . (<> [WhileInPlay (toTarget thisAlly) [AllyEnteredPlayUnderController aid, ResolveSurge (toTarget thisAlly) card]])
          =<< revealing (toTarget thisAlly)
        pure $ g & entitiesL . alliesL %~ insertEntity thisAlly & playersL . ix owner.id . setAsideCardsL %~ filter ((/= card.id) . (.id))
      -- a player event with the Encounter keyword, as Wind from the Sea: its When Revealed,
      -- which cannot be canceled (MEC61 rules), is resolved by the event, built from its card
      EventType | any (any ((== card.id) . (.id)) . (.setAsideCards)) (toList g.players) -> do
        pushAll =<< revealing (CardIdTarget card.id)
        pure g
      -- its doomed keyword resolves too, as Covered in Darkness's
      t | t `elem` [ObjectiveType, ObjectiveAllyType] -> do
        -- an objective ally a player gains control of, as Gildor Inglorion, goes to him
        let comesOut = if gainsController def then AddToStaging card else PutObjectiveIntoPlay card
        pushAll . (<> doomed <> [ResolveSurge (CardIdTarget card.id) card]) =<< inLogBlock (RevealBlock card) [comesOut]
        pure g
      -- an encounter side quest goes to the staging area, where the players may quest
      -- against it
      EncounterSideQuestType -> do
        sideQuest <- newQuest card
        pushAll . (<> [ResolveSurge (toTarget sideQuest) card]) =<< revealing (toTarget sideQuest)
        pure $ g & entitiesL . questsL %~ insertEntity sideQuest
      _ -> do
        pushAll =<< inLogBlock (RevealBlock card) []
        pure $ g & scenarioL %~ overAttrs (encounterDiscardL %~ (card :))
  ResolveWhenRevealedOf card -> do
    let def = toCardDef card
        whenRevealed t = inLogBlock (WhenRevealedBlock card) [if cdWhenRevealed def then AskToResolveWhenRevealed t card else WhenRevealed t]
    case cdCardType def of
      TreacheryType -> do
        treachery <- newTreachery card
        pushAll . (<> [FinishRevelation (toId treachery)]) =<< whenRevealed (toTarget treachery)
        pure $ g & entitiesL . treacheriesL %~ insertEntity treachery
      -- an enemy or location goes to the staging area, as each card resolving one this
      -- way reads (The North Stair, Lying in Wait's locations); one wanting it elsewhere
      -- would move it after
      EnemyType -> do
        enemy <- newEnemy card
        pushAll =<< whenRevealed (toTarget enemy)
        pure $ g & entitiesL . enemiesL %~ insertEntity (overAttrs (\a -> a {enemyPlacement = StagingArea}) enemy)
      LocationType -> do
        location <- newLocation card
        pushAll =<< whenRevealed (toTarget location)
        pure $ g & entitiesL . locationsL %~ insertEntity (overAttrs (\a -> a {locationPlacement = StagingArea}) location)
      _ -> pure $ g & scenarioL %~ overAttrs (encounterDiscardL %~ (card :))
  -- a set-aside player event with the Encounter keyword, revealed: it leaves play once
  -- resolved, and so is removed from the game (MEC61 rules)
  WhenRevealed (CardIdTarget cid) | Just owner <- find (any ((== cid) . (.id)) . (.setAsideCards)) (toList g.players) -> do
    for_ [c | c <- owner.setAsideCards, c.id == cid] \c -> for_ (lookupEvent (toCardCode c) cid owner.id) $ void . runEach msg
    pure $ g & playersL . ix owner.id . setAsideCardsL %~ filter ((/= cid) . (.id))
  -- an enemy destroyed as it was revealed resolves no more of its revelation (FAQ)
  AskToResolveWhenRevealed t _ | not (inPlay g t) -> pure g
  AskToResolveWhenRevealed t card -> do
    lead <- getLead
    push
      $ questionLabelWith
        (ikeyWith "label.resolveWhenRevealed" (cardVar "card" card))
        (Just t)
        []
        lead
        [targetLabel t [WhenRevealed t]]
    pure g
  ResolveSurge t card -> do
    surges <- hasKeyword Surge t (toCardDef card)
    pushWhen surges $ SurgeReveal card.id
    pure $ if surges then g else g & surgeChainL .~ []
  -- In the quest phase the encounter deck is reset as it empties (after the staging of
  -- its last card: see 'HeldReset'), so a deck of surging treacheries alone, each
  -- discarded before its surge reveals the next, would be revealed again and again: the
  -- rules as written never end the chain. The engine stops it once every card of the
  -- encounter deck has surged in it, each having been revealed once. No ruling covers
  -- this; it is a stop the game needs, not one a rule or FAQ gives.
  SurgeReveal cid -> do
    let chain = cid : gameSurgeChain g
        deck = encounterDeckAfterHeldReset (toAttrs g.scenario)
    if notNull deck && all ((`elem` chain) . (.id)) deck
      then pure $ g & surgeChainL .~ []
      else do
        push RevealEncounterCard
        pure $ g & surgeChainL .~ chain
  FinishRevelation tid -> do
    -- a treachery that did not attach itself to something is discarded once it resolves
    for_ (Map.lookup tid g.entities.treacheries) \treachery ->
      when (treachery.placement == Unplaced) $ push (Discard $ toTarget treachery)
    pushAll $ freeGuarded g (TreacheryTarget tid)
    -- its effects have resolved or been canceled, so it guards nothing any more
    let cid = (.cardId) . toAttrs <$> Map.lookup tid g.entities.treacheries
        guarding o = isJust cid && (toAttrs o).objectiveGuard == cid
    pure
      $ g
      & entitiesL
      . objectivesL
      . traverse
      %~ (\o -> if guarding o then overAttrs (\a -> a {objectiveGuard = Nothing}) o else o)
  -- a card out of play comes into play as a captive: see 'GuardCard'
  -- an ally card no longer in its owner's discard pile, taken already, stays where it is
  GuardCard card cid -> do
    aid <- getRandom
    lead <- getLead
    let (owner, available) = case card of
          PlayerCard pc -> case pc.owner of
            Just o -> (o, maybe False (any ((== pc.id) . (.id)) . (.discard)) (Map.lookup o g.players))
            Nothing -> (lead, False)
          EncounterCard _ -> (lead, True)
        captive = overAttrs (\a -> a {allyPlacement = AttachedTo (CardIdTarget cid)}) $ lookupAlly (toCardCode card) card.id aid owner
    if available
      then do
        pushAll [RemoveFromDiscard owner card.id | PlayerCard _ <- [card]]
        pure $ g & entitiesL . alliesL %~ insertEntity captive
      else pure g
  -- as a Ship objective of the fleet comes into play (Dream-chaser rules)
  PutObjectiveAllyIntoPlayUnder pid card -> do
    aid <- getRandom
    thisAlly <- entering $ overAttrs (\a -> a {allyPlacement = InPlayArea pid}) $ lookupAlly (toCardCode card) card.id aid pid
    push $ enteredPlayThisRound thisAlly
    push $ CheckWindows [mkAfter $ EntersPlay $ toTarget thisAlly]
    pure $ g & entitiesL . alliesL %~ insertEntity thisAlly
  -- a card whose other side is another kind of card turns over, as Captain Sahír from enemy
  -- to objective-ally and back: it does not leave play, but what was on it is discarded and
  -- its tokens go, and the side it turns to is under the player that side names
  Flip (EnemyTarget eid)
    | Just e <- Map.lookup eid g.entities.enemies
    , Just other <- cdOtherSide (toCardDef e)
    , cdCardType (toCardDef other) == ObjectiveAllyType -> do
        pid <- objectiveAllyController (toCardDef other)
        pushAll $ discardAttached g (EnemyTarget eid)
        let thisAlly = overAttrs (\a -> a {allyPlacement = InPlayArea pid}) $ lookupAlly other (toCard e).id (AllyId $ unEnemyId eid) pid
        pure $ g & entitiesL . enemiesL %~ Map.delete eid & entitiesL . alliesL %~ insertEntity thisAlly
  -- and back to its enemy side, in the staging area, entering play, as Gollum does to
  -- engage the first player (The Land of Shadow rules insert)
  Flip (AllyTarget aid)
    | Just a <- Map.lookup aid g.entities.allies
    , Just other <- cdOtherSide (toCardDef a)
    , cdCardType (toCardDef other) == EnemyType -> do
        pushAll $ discardAttached g (AllyTarget aid)
        let thisEnemy = overAttrs (\e -> e {enemyPlacement = StagingArea}) $ lookupEnemy other (toCard a).id (EnemyId $ unAllyId aid)
        push $ CheckWindows [mkAfter $ EntersPlay $ toTarget thisEnemy]
        pure $ g & entitiesL . alliesL %~ Map.delete aid & entitiesL . enemiesL %~ insertEntity thisEnemy
  -- a hero whose other side is an enemy turns over, as Sméagol to Gollum: the hero leaves
  -- play, and the enemy enters play engaged with its owner, ready, still its owner's card
  -- (MEC77 rules, "Sméagol / Gollum")
  Flip target@(HeroTarget hid)
    | Just h <- Map.lookup hid g.entities.heroes
    , Just other <- cdOtherSide (toCardDef h)
    , cdCardType (toCardDef other) == EnemyType -> do
        g' <- leftPlay g target
        pushAll $ discardAttached g target
        eid <- getRandom
        let thisEnemy = overAttrs (\e -> e {enemyPlacement = EngagedWith h.owner, enemyOwner = Just h.owner}) $ lookupEnemy other (toCard h).id eid
        push $ CheckWindows [mkAfter $ EntersPlay $ toTarget thisEnemy]
        pure $ g' & entitiesL . heroesL %~ Map.delete hid & entitiesL . enemiesL %~ insertEntity thisEnemy
  -- and back: the enemy leaves play, and the hero enters play under its owner's control,
  -- exhausted (MEC77 rules)
  Flip target@(EnemyTarget eid)
    | Just e <- Map.lookup eid g.entities.enemies
    , Just other <- cdOtherSide (toCardDef e)
    , cdCardType (toCardDef other) == HeroType
    , Just owner <- enemyOwner (toAttrs e) -> do
        pushAll $ discardAttached g target
        hid <- getRandom
        let thisHero = overAttrs (\a -> a {heroExhausted = True}) $ lookupHero other (toCard e).id hid owner
        push $ CheckWindows [mkAfter $ EntersPlay $ toTarget thisHero]
        pure $ g & entitiesL . enemiesL %~ Map.delete eid & entitiesL . heroesL %~ insertEntity thisHero
  -- a location whose other side is an objective ally, as Woodmen Village's Haldan, turns
  -- over out of the locations: the effect turning it puts that side where it says, under
  -- the location's card id
  Flip target@(LocationTarget lid)
    | Just l <- Map.lookup lid g.entities.locations
    , Just other <- cdOtherSide (toCardDef l)
    , cdCardType (toCardDef other) == ObjectiveAllyType -> do
        pushAll $ discardAttached g target
        pure $ g & entitiesL . locationsL %~ Map.delete lid
  -- a location whose other side is an enemy turns over, as The Two Watchers does after the
  -- players travel there: the location leaves play, what was on it discarded, and the
  -- enemy, the same card, is moved to the staging area
  Flip target@(LocationTarget lid)
    | Just l <- Map.lookup lid g.entities.locations
    , Just other <- cdOtherSide (toCardDef l)
    , cdCardType (toCardDef other) == EnemyType -> do
        pushAll $ discardAttached g target
        eid <- getRandom
        let thisEnemy = overAttrs (\e -> e {enemyPlacement = StagingArea}) $ lookupEnemy other (toCard l).id eid
        push $ CheckWindows [mkAfter $ EntersStagingArea (toTarget thisEnemy)]
        pure $ g & entitiesL . locationsL %~ Map.delete lid & entitiesL . enemiesL %~ insertEntity thisEnemy
  -- a double-sided location turns over, as an Uncharted location does
  Flip (LocationTarget lid) | Just l <- turnLocationOver =<< Map.lookup lid g.entities.locations -> do
    pushAll $ [ArrangeIslandMap | isJust l.position] <> [CheckWindows [mkAfter $ LocationFlipped lid]]
    pure $ g & entitiesL . locationsL %~ Map.insert lid l
  -- as Shrine to Morgoth floods: turned over, its progress back on the side it turns to
  FlipKeepingProgress lid | Just l <- Map.lookup lid g.entities.locations, Just l' <- turnLocationOver l -> do
    push $ CheckWindows [mkAfter $ LocationFlipped lid]
    let progress = countTokens Progress (locationTokens $ toAttrs l)
    pure $ g & entitiesL . locationsL %~ Map.insert lid (overAttrs (\a -> a {locationTokens = addTokens Progress progress (locationTokens a)}) l')
  AddFromGrottoDeck n -> do
    let (top, rest) = splitAt n (toAttrs g.scenario).grottoDeck
    pushAll $ map AddToStaging top
    pure $ g & scenarioL %~ overAttrs (grottoDeckL .~ rest)
  -- the card enters play with its other side faceup
  AddUnderwaterFromGrottoDeck -> case (toAttrs g.scenario).grottoDeck of
    top : rest -> do
      push $ AddToStaging $ maybe top (MkEncounterCard top.id) (cdOtherSide $ toCardDef top)
      pure $ g & scenarioL %~ overAttrs (grottoDeckL .~ rest)
    [] -> pure g
  PutOnBottomOfGrottoDeck card -> do
    let def = toCardDef card
        grotto = case cdOtherSide def of
          Just other | Grotto `notMember` cdCardTraits def -> MkEncounterCard card.id other
          _ -> card
    pure $ g & scenarioL %~ overAttrs (grottoDeckL %~ (<> [grotto]))
  ShuffleIntoGrottoDeck lid | Just l <- Map.lookup lid g.entities.locations -> do
    pushAll $ discardAttached g (LocationTarget lid)
    deck <- shuffleM (grottoDeckCard (toAttrs l) : (toAttrs g.scenario).grottoDeck)
    pure $ g & entitiesL . locationsL %~ Map.delete lid & scenarioL %~ overAttrs (grottoDeckL .~ deck)
  LookAtFacedownSide pid lid ->
    pure $ g & entitiesL . locationsL . ix lid %~ overAttrs (\a -> a {locationLookedAt = ordNub (pid : a.lookedAt)})
  AddFromUnchartedDeck n -> do
    let (top, rest) = splitAt n (toAttrs g.scenario).unchartedDeck
    pushAll $ map AddToStaging top
    pure $ g & scenarioL %~ overAttrs (unchartedDeckL .~ rest)
  ShuffleIntoUnchartedDeck lid | Just l <- Map.lookup lid g.entities.locations -> do
    pushAll $ discardAttached g (LocationTarget lid)
    deck <- shuffleM (locationDeckCard (toAttrs l) : (toAttrs g.scenario).unchartedDeck)
    pure $ g & entitiesL . locationsL %~ Map.delete lid & scenarioL %~ overAttrs (unchartedDeckL .~ deck)
  -- each keeps its place in the staging area, but the cards are dealt among them afresh,
  -- each with a new card id, so nothing seen of one follows its card
  ShuffleUnchartedLocations -> do
    let staged = [l | l <- toList g.entities.locations, l.placement == StagingArea, isUncharted l]
    codes <- shuffleM $ map (toCardCode . locationDeckCard . toAttrs) staged
    dealt <- for (zip staged codes) \(l, code) -> do
      cid <- getRandom
      pure $ overAttrs (\a -> a {locationPlacement = StagingArea, locationPosition = l.position}) $ enterFaceUp $ createLocation (MkEncounterCard cid code) (toId l)
    pure $ g & entitiesL . locationsL %~ \ls -> foldr (\l -> Map.insert (toId l) l) ls dealt
  PrepareIslandMap cards -> do
    laid <- for cards \(pos, card) -> overAttrs (\a -> a {locationPlacement = OnIslandMap, locationPosition = Just pos}) <$> newLocation card
    pure $ g & entitiesL . locationsL %~ \ls -> foldr (\l -> Map.insert (toId l) l) ls laid
  SwapIslandMapPositions a b
    | Just la <- Map.lookup a g.entities.locations
    , Just lb <- Map.lookup b g.entities.locations -> do
        push ArrangeIslandMap
        let moveTo l = overAttrs (\x -> x {locationPosition = l.position})
        pure $ g & entitiesL . locationsL %~ Map.adjust (moveTo lb) a . Map.adjust (moveTo la) b
  ArrangeIslandMap -> do
    let laid = filter (isJust . (.position)) (toList g.entities.locations)
        actives = [p | l <- laid, l.placement == ActiveLocation, Just p <- [l.position]]
        aroundActive l = maybe False (\p -> any ((p `elem`) . adjacentPositions) actives) l.position
    placements <- for laid \l -> do
      excluded <- elem NotInStagingArea <$> getModifiers (toTarget l)
      pure
        ( toId l
        , if
            | l.placement == ActiveLocation -> ActiveLocation
            | aroundActive l && not excluded -> StagingArea
            | otherwise -> OnIslandMap
        )
    pure $ g & entitiesL . locationsL %~ \ls -> foldr (\(lid, p) -> Map.adjust (overAttrs (\a -> a {locationPlacement = p})) lid) ls placements
  PutObjectiveAllyIntoPlay oid pid | Just o <- Map.lookup oid g.entities.objectives -> do
    thisAlly <-
      entering
        -- the damage it took in the staging area stays on it, as Celador's
        $ overAttrs (\a -> a {allyPlacement = InPlayArea pid, allyTokens = addTokens Damage (countTokens Damage $ objectiveTokens $ toAttrs o) mempty})
        $ lookupAlly (toCardCode o) (toCard o).id (AllyId $ unObjectiveId oid) pid
    push $ enteredPlayThisRound thisAlly
    push $ CheckWindows [mkAfter $ EntersPlay $ toTarget thisAlly]
    pure $ g & entitiesL . objectivesL %~ Map.delete oid & entitiesL . alliesL %~ insertEntity thisAlly
  ShuffleIntoEncounterDeck target@(LocationTarget lid) | Just l <- Map.lookup lid g.entities.locations -> do
    pushAll $ [Discard (toTarget a) | a <- toList g.entities.attachments, a.placement == AttachedTo target] <> freeGuarded g target
    deck <-
      shuffleM
        (MkEncounterCard (locationCardId $ toAttrs l) (toCardCode l) : (toAttrs g.scenario).encounterDeck)
    pure
      $ g
      & entitiesL
      . locationsL
      %~ Map.delete lid
      & scenarioL
      %~ overAttrs (encounterDeckL .~ deck)
  ShuffleIntoEncounterDeck (TreacheryTarget tid) | Just t <- Map.lookup tid g.entities.treacheries -> do
    deck <-
      shuffleM
        (MkEncounterCard (treacheryCardId $ toAttrs t) (toCardCode t) : (toAttrs g.scenario).encounterDeck)
    pure
      $ g
      & entitiesL
      . treacheriesL
      %~ Map.delete tid
      & scenarioL
      %~ overAttrs (encounterDeckL .~ deck)
  DrawTreacheryIntoHand pid tid | Just t <- Map.lookup tid g.entities.treacheries -> do
    let card = EncounterCard $ MkEncounterCard (treacheryCardId $ toAttrs t) (toCardCode t)
    pure $ g & entitiesL . treacheriesL %~ Map.delete tid & playersL . ix pid . handL %~ (<> [card])
  -- an enemy leaves play for the encounter deck, its attachments and shadow cards discarded
  ShuffleIntoEncounterDeck target@(EnemyTarget eid) | Just e <- Map.lookup eid g.entities.enemies -> do
    pushAll
      $ [Discard (toTarget a) | a <- toList g.entities.attachments, a.placement == AttachedTo target]
      <> [Discard (toTarget t) | t <- toList g.entities.treacheries, t.placement == AttachedTo target]
      <> attachedEnemies g target
      <> [AddToEncounterDiscard e.shadowCards | notNull e.shadowCards]
      <> freeGuarded g target
    let card = MkEncounterCard (enemyCardId $ toAttrs e) (toCardCode e)
        removed = g & entitiesL . enemiesL %~ Map.delete eid
    toPlayersDeck <- elem LeavesPlayToAnyPlayersDeck <$> getModifiers target
    if toPlayersDeck
      then removed <$ toAnyPlayersDeck g card
      else do
        deck <- shuffleM (card : (toAttrs g.scenario).encounterDeck)
        pure $ removed & scenarioL %~ overAttrs (encounterDeckL .~ deck)
  -- an encounter card drawn into a hand is answered by its card, out of play, as Recurring
  -- Nightmare's Forced effect (as Arkham's drawn encounter cards resolve their revelations)
  EncounterCardEnteredHand _ card | cdCardType (toCardDef card) == EnemyType -> do
    _ <- runEach msg . createEnemy card =<< getRandom
    pure g
  -- and a treachery drawn, as The Searching Eye's "After you draw this card"
  EncounterCardDrawn _ card | cdCardType (toCardDef card) == TreacheryType -> do
    _ <- runEach msg . createTreachery card =<< getRandom
    pure g
  AddToStaging card | gainsController (toCardDef card) -> do
    thisObjective <- newObjective card
    controller <- objectiveAllyController (toCardDef card)
    push $ PutObjectiveAllyIntoPlay (toId thisObjective) controller
    pure
      $ g
      & entitiesL
      . objectivesL
      %~ insertEntity (overAttrs (\a -> a {objectivePlacement = StagingArea}) thisObjective)
  AddToStaging card -> case cdCardType (toCardDef card) of
    EnemyType -> do
      thisEnemy <- newEnemy card
      pushAll [CheckWindows [mkAfter $ EntersStagingArea (toTarget thisEnemy)], AmbushEngagementCheck (toId thisEnemy)]
      pure
        $ g
        & entitiesL
        . enemiesL
        %~ insertEntity (overAttrs (\a -> a {enemyPlacement = StagingArea}) thisEnemy)
    LocationType -> do
      thisLocation <- newLocation card
      push $ CheckWindows [mkAfter $ EntersStagingArea (toTarget thisLocation)]
      -- Guarded (X) resolves as the card enters play, not only as it is revealed (MEC65
      -- rules), as Searching for a Way Out puts an objective-location into play
      pushAll [GuardLocationBy (toId thisLocation) ts | GuardedBy ts <- toList (cdKeywords $ toCardDef card)]
      pure
        $ g
        & entitiesL
        . locationsL
        %~ insertEntity (overAttrs (\a -> a {locationPlacement = StagingArea}) thisLocation)
    EncounterSideQuestType -> do
      sideQuest <- newQuest card
      push $ CheckWindows [mkAfter $ EntersStagingArea (toTarget sideQuest)]
      pure $ g & entitiesL . questsL %~ insertEntity sideQuest
    _ -> do
      thisObjective <- newObjective card
      push $ CheckWindows [mkAfter $ EntersStagingArea (toTarget thisObjective)]
      pure
        $ g
        & entitiesL
        . objectivesL
        %~ insertEntity (overAttrs (\a -> a {objectivePlacement = StagingArea}) thisObjective)
  _ -> pure g

