module LordOfTheRings.Quest.Cards.CarriedAway (carriedAway) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card (CardType (EnemyType), cdCardType, toCard, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Hero.Types (HeroAttrs (heroGuardedBy, heroTokens))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.LostInMirkwood.Helpers (advancingFromStage)

newtype CarriedAway = CarriedAway QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Stage 3A of Lost in Mirkwood. No quest points.
carriedAway :: QuestCard CarriedAway
carriedAway = quest CarriedAway Cards.carriedAway

{- | Each enemy guarding a hero cannot take non-combat damage. Each guarded hero is in play
but under no player's control: a captive of its guard (see 'guardCharacterWith').
-}
instance HasModifiersFor CarriedAway where
  getModifiersFor (CarriedAway a) = when a.flipped do
    enemies <- filterM (fmap (any isHero) . charactersGuardedBy . (.id) . toCard) =<< getsGame (toList . (.entities.enemies))
    for_ enemies \e -> modified a e [CannotTakeNonCombatDamage]
   where
    isHero = \case
      HeroTarget _ -> True
      _ -> False

{- | Forced: When an enemy guarding a hero is defeated, return that hero to its owner's
control exhausted: exhausted now, it is rescued as its guard leaves play.

A guard that leaves play another way, discarded or shuffled away, frees the hero too: an
enemy guarding an objective is dealt with as it "leaves play, either by being defeated or
as the result of a card effect" (Rules Reference, "Guarded"). The hero is then no longer a
guarded hero, so no longer under no player's control, and its owner has it back; only
defeat exhausts it, as this Forced effect says.
-}
instance HasAbilities CarriedAway where
  getAbilities (CarriedAway a) = [mkAbility a 1 $ forced $ MatchingEnemyDefeatedWindow When EnemyGuardingAHero | a.flipped]

{- | When Revealed: Each player chooses a hero he controls, discards each token and
attachment from that hero, and places it in the staging area. Each player discards cards
from the top of the encounter deck until an enemy is discarded and attaches that enemy to
the hero he owns in the staging area, guarding it.
When the players advance from this stage, discard each guarded hero.
-}
instance RunMessage CarriedAway where
  runMessage msg q@(CarriedAway attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      CarriedAway <$> runMessage msg attrs
    -- one player after another, each discarding from the deck as the one before left it
    WhenRevealed (isTarget attrs -> True) -> do
      doStep 1 msg
      pure q
    DoStep n (WhenRevealed (isTarget attrs -> True)) -> do
      players <- getRemainingPlayers
      for_ (drop (n - 1) players & take 1) \p -> do
        heroes <- select (HeroControlledBy $ PlayerWithId p)
        chooseOrRunOneM p do
          questionLabeled "carriedAway.hero"
          causedBy attrs
          targets heroes \h -> do
            hero <- getHero h
            for_ (Map.toList $ heroTokens $ toAttrs hero) \(token, k) -> removeTokens attrs h token k
            traverse_ discard =<< getAttachmentsOn (toTarget h)
            moveHeroToStagingArea h
            discardEncounterCardsUntilFor attrs h (CardWithType EnemyType)
        doStep (n + 1) (WhenRevealed $ toTarget attrs)
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) (HeroTarget h) cards -> do
      for_ (find isEnemy cards) \e -> do
        addToStagingFromDiscard e
        guardCharacterWith h e
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [eid | Window _ (EnemyDefeated eid) <- ws] \eid -> do
        captor <- getEnemy eid
        heroes <- charactersGuardedBy (toCard captor).id
        for_ [h | h@(HeroTarget _) <- heroes] exhaust
      pure q
    _ | advancingFromStage msg -> do
      guarded <- getsGame (filter (isJust . heroGuardedBy . toAttrs) . toList . (.entities.heroes))
      for_ guarded discard
      CarriedAway <$> runMessage msg attrs
    _ -> CarriedAway <$> runMessage msg attrs
   where
    isEnemy = (== EnemyType) . cdCardType . toCardDef
