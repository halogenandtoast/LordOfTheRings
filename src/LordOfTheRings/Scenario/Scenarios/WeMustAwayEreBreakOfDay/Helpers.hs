-- | What We Must Away, Ere Break of Day's cards share: the Sack cards and their When Sacked
-- effects, and the Troll Purse and Troll Key the Trolls carry.
module LordOfTheRings.Scenario.Scenarios.WeMustAwayEreBreakOfDay.Helpers (
  sackModifiers,
  sackOntoHighest,
  sackableCharacters,
  sackableAllies,
  sackableHeroes,
  printedStat,
  trollEnemy,
  trollTreasureAbilities,
  runTrollTreasure,
  attachToATroll,
  bilboHas,
  trollAbilities,
  runTroll,
) where

import LordOfTheRings.Ability
import LordOfTheRings.Card (CardDef (cdName), toCardCode)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasModifiersFor
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Enemy.Runner (EnemyAttrs)
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Helpers.Character (characterCardDef)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Modifier
import LordOfTheRings.Name (Name (..))
import LordOfTheRings.Objective.Runner
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Placement (Placement (AttachedTo, EngagedWith, StagingArea))
import LordOfTheRings.Prelude
import LordOfTheRings.Queue
import LordOfTheRings.Source (isSource)
import LordOfTheRings.Target
import LordOfTheRings.Timing (Timing (After, When))
import LordOfTheRings.Trait (Trait (Sack, Troll))

-- | "Attached character cannot ready, attack, defend, commit to quests, or trigger
-- effects. If this Sack card is removed, shuffle it into the sack deck." As any Sack card
-- leaving play is (We Must Away, Ere Break of Day rules).
sackModifiers :: HasModifiersM m => ObjectiveAttrs -> m ()
sackModifiers a = do
  modifySelf a [LeavesPlayToSackDeck]
  withHost a \host -> modified a host [CannotReady, CannotAttackOrDefend, CannotCommitToQuest, CannotTriggerAbilities]

{- | When Sacked: attach the Sack card to the eligible character with the highest value;
the first player chooses among those tied. With none eligible, it is shuffled back into
the sack deck (We Must Away, Ere Break of Day rules, "Sack X").
-}
sackOntoHighest :: ReverseQueue m => ObjectiveAttrs -> [(Target, Int)] -> m ()
sackOntoHighest a scored = case maximumMay (map snd scored) of
  Nothing -> returnToSackDeck a.id
  Just best -> do
    lead <- getLead
    chooseOrRunOneM lead do
      questionLabeled "sack.attach"
      causedBy a
      targets [t | (t, n) <- scored, n == best] \t -> placeObjective a.id (AttachedTo t)

-- | The characters a Sack card may be attached to: not Gandalf, without a Sack card
-- attached, and able to have attachments. A target it cannot be attached to is not
-- eligible, and the first player must choose an eligible one (rules, "Sack X").
sackableCharacters :: HasGame m => m [Target]
sackableCharacters = sackable =<< selectCharacters (HeroControlledBy AnyPlayer) (AllyControlledBy AnyPlayer)

sackableAllies :: HasGame m => m [Target]
sackableAllies = sackable . map toTarget =<< select (AllyControlledBy AnyPlayer)

-- | A Worn Sack's heroes: it does not exclude Gandalf.
sackableHeroes :: HasGame m => m [Target]
sackableHeroes = filterM unsacked . map toTarget =<< select (HeroControlledBy AnyPlayer)

sackable :: HasGame m => [Target] -> m [Target]
sackable = filterM \t -> andM [not <$> isGandalf t, unsacked t]
 where
  isGandalf t = (\(Name title _) -> title == "Gandalf") . cdName <$> characterCardDef t

unsacked :: HasGame m => Target -> m Bool
unsacked t = do
  sacked <- notNull <$> attachedWithTrait Sack t
  barred <- elem CannotHaveAttachments <$> getModifiers t
  pure $ not sacked && not barred

-- | The character's printed value, as "the highest printed attack" reads: 0 for none.
printedStat :: HasGame m => (CardDef -> Maybe Int) -> Target -> m Int
printedStat stat = fmap (fromMaybe 0 . stat) . characterCardDef

trollEnemy :: EnemyMatcher
trollEnemy = EnemyWithTrait Troll

{- | "If unattached and in the staging area, attach it to a Troll enemy, if able.": as it
enters the staging area, or a Troll enemy enters play while it waits there.
-}
trollTreasureAbilities :: ObjectiveAttrs -> [Ability]
trollTreasureAbilities a =
  [ restricted a 1 (exists trollEnemy)
      $ silent
      $ AnyWindowOf [CardEntersStagingAreaWindow After (toTarget a), MatchingEnemyEntersPlayWindow After trollEnemy]
  | a.placement == StagingArea
  ]

-- | The first player attaches the card to a Troll enemy in play, if there is one.
attachToATroll :: ReverseQueue m => ObjectiveAttrs -> m ()
attachToATroll a = do
  trolls <- select trollEnemy
  lead <- getLead
  chooseOrRunOneM lead do
    questionLabeled "trollTreasure.attach"
    causedBy a
    targets trolls \t -> placeObjective a.id (AttachedTo $ toTarget t)

-- | Ability 1: attach to a Troll. "If discarded, add it to the staging area" is what any
-- objective does.
runTrollTreasure :: Message -> ObjectiveAttrs -> QueueT Message GameT ObjectiveAttrs
runTrollTreasure msg a = case msg of
  UseCardAbility _ (isSource a -> True) 1 _ -> do
    attachToATroll a
    pure a
  _ -> runMessage msg a

-- | Whether Bilbo Baggins has the objective attached, as Troll Cave asks of the Troll Key
-- and the Troll Purse.
bilboHas :: HasGame m => CardDef -> m Bool
bilboHas def = selectAny (HeroWithTitle "Bilbo Baggins" <> HeroWithAttachment (toCardCode def))

{- | A Troll's Forced effects: "After this enemy engages a player, sack N." (1) and "Return
this enemy to the staging area at the end of the combat phase. The engaged player may raise
his threat by 1 to cancel this effect." (2)
-}
trollAbilities :: EnemyAttrs -> [Ability]
trollAbilities a =
  [ mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id
  , restricted a 2 (exists $ EnemyWithId a.id <> EnemyEngagedWith AnyPlayer) $ forced $ PhaseEndsWindow When CombatPhase
  ]

runTroll :: Int -> Message -> EnemyAttrs -> QueueT Message GameT EnemyAttrs
runTroll n msg a = case msg of
  UseCardAbility _ (isSource a -> True) 1 _ -> do
    sack n
    pure a
  UseCardAbility _ (isSource a -> True) 2 _ -> do
    case a.placement of
      EngagedWith pid -> chooseOneM pid do
        questionLabeled "troll.returnToStaging"
        causedBy a
        labeled "troll.raiseThreat" $ raiseThreat pid 1
        labeled "troll.return" $ placeEnemy a.id StagingArea
      _ -> pure ()
    pure a
  _ -> runMessage msg a
