module LordOfTheRings.Treachery.Cards.FallenIntoEvil (fallenIntoEvil) where

import LordOfTheRings.Card (CardDef (..), toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.BreakingOfTheFellowship.Helpers (toFirstPlayersStagingArea)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FallenIntoEvil = FallenIntoEvil TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

fallenIntoEvil :: TreacheryCard FallenIntoEvil
fallenIntoEvil = treachery FallenIntoEvil Cards.fallenIntoEvil

{- | Counts as a Condition attachment with the text: "Attached hero loses the hero card type
and gains the enemy card type with threat equal to its willpower and engagement cost equal
to its threat cost." Its printed willpower and threat cost: an enemy has no willpower to
modify.
-}
instance HasModifiersFor FallenIntoEvil where
  getModifiersFor (FallenIntoEvil a) = case a.placement of
    AttachedTo host@(EnemyTarget eid) -> do
      fallen <- getsGame (fmap toCardDef . lookup eid . (.entities.enemies))
      for_ fallen \def ->
        modified a host [ThreatModifier (fromMaybe 0 $ cdWillpower def), EngagementCostModifier (fromMaybe 0 $ cdThreatCost def)]
    _ -> pure ()

{- | When Revealed: Attach to a hero and move it to the first player's staging area. The
player who revealed it chooses the hero. "If Fallen into Evil is discarded, return attached
hero to its owner's control": unless the hero turned enemy has left play first, as it does
once destroyed.
-}
instance RunMessage FallenIntoEvil where
  runMessage msg t@(FallenIntoEvil attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      heroes <- select (HeroControlledBy AnyPlayer)
      chooseOrRunOneM you do
        questionLabeled "fallenIntoEvil.attach"
        causedBy attrs
        targets heroes \h -> do
          let eid = EnemyId (unHeroId h)
          heroBecomesEnemy h
          attachTreachery attrs.treacheryId eid
          toFirstPlayersStagingArea eid
      pure t
    Discard _ (isTarget attrs -> True) -> do
      case attrs.placement of
        AttachedTo (EnemyTarget eid) -> whenM (selectAny $ EnemyWithId eid) $ enemyBecomesHero eid
        _ -> pure ()
      FallenIntoEvil <$> runMessage msg attrs
    _ -> FallenIntoEvil <$> runMessage msg attrs
