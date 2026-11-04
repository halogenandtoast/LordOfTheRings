module LordOfTheRings.Enemy.Cards.WatcherInTheWater (watcherInTheWater) where

import LordOfTheRings.Card (toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (RingBearer))

newtype WatcherInTheWater = WatcherInTheWater EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

watcherInTheWater :: EnemyCard WatcherInTheWater
watcherInTheWater = enemy WatcherInTheWater Cards.watcherInTheWater

{- | Indestructible. Immune to player card effects. Cannot leave the staging area but is
considered to be engaged with each player: attacked there, and attacking each player in
the combat phase, as Durin's Bane.
-}
instance HasModifiersFor WatcherInTheWater where
  getModifiersFor (WatcherInTheWater a) =
    modifySelf a [ImmuneToPlayerCardEffects, CannotLeaveStagingArea, CannotBeOptionallyEngagedBy AnyPlayer, EngagedWithEachPlayer AnyPlayer]

{- | Forced: After placing the 6th damage here, the first player takes control of the
Ring-bearer, exhausted with 1 damage on it. The Ring-bearer is facedown under it, its
captive, from stage 4A on; freed, he is the first player's, his attachments with him.
-}
instance RunMessage WatcherInTheWater where
  runMessage msg (WatcherInTheWater attrs) = case msg of
    PlaceTokens _ (isTarget attrs -> True) Damage _ -> do
      attrs' <- runMessage msg attrs
      when (attrs.damage < 6 && attrs'.damage >= 6) do
        lead <- getLead
        captives <- charactersGuardedBy attrs.cardId
        for_ [h | HeroTarget h <- captives] \h -> do
          hero <- getHero h
          ringBearer <- hasTrait RingBearer (toTarget h) (toCardDef hero)
          when ringBearer do
            rescueCharacter h
            pushWhen (hero.owner /= lead) $ ChangeHeroController h lead
            exhaust h
            placeTokens attrs h Damage 1
      pure $ WatcherInTheWater attrs'
    _ -> WatcherInTheWater <$> runMessage msg attrs
