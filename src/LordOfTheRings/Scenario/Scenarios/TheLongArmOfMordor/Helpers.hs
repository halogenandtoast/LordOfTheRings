-- | What the cards of The Long Arm of Mordor share: its objective-heroes, and the heroes
-- waiting in the staging area.
module LordOfTheRings.Scenario.Scenarios.TheLongArmOfMordor.Helpers (
  eliminatedIfLeavesPlay,
  takeHeroFromStagingArea,
  isObjectiveHero,
  stagingHeroRules,
) where

import LordOfTheRings.Card (CardType (ObjectiveHeroType), cdCardType, toCardDef)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Game.Base (gameEffects)
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Leadership, Lore, Spirit, Tactics))

{- | "If (this hero) leaves play, you are eliminated from the game." This text cannot be
modified (MEC55 rules), so it is no ability a blank text box could lose: its owner is
eliminated as it is discarded, by damage or a card, or removed from the game, unless he
already is, as when his elimination discards it.
-}
eliminatedIfLeavesPlay :: ReverseQueue m => HeroAttrs -> Message -> m ()
eliminatedIfLeavesPlay attrs = \case
  Discard t | isTarget attrs t -> leaves
  RemoveFromGame t | isTarget attrs t -> leaves
  _ -> pure ()
 where
  leaves = do
    owner <- getPlayer attrs.owner
    unless owner.eliminated $ eliminatePlayer attrs.owner

-- | "Each player chooses a hero he owns in the staging area, takes control of that hero,
-- and exhausts it."
takeHeroFromStagingArea :: (ReverseQueue m, Targetable source) => source -> PlayerId -> m ()
takeHeroFromStagingArea source pid = do
  heroes <- filterM (fmap ((== pid) . (.owner)) . getHero) =<< select HeroInStagingArea
  chooseOrRunOneM pid do
    questionLabeled "theLongArmOfMordor.takeControl"
    causedBy source
    targets heroes takeControlOfHero

-- | Whether the character is an objective-hero.
isObjectiveHero :: HasGame m => Target -> m Bool
isObjectiveHero = \case
  HeroTarget hid -> (== ObjectiveHeroType) . cdCardType . toCardDef <$> getHero hid
  _ -> pure False

{- | Stages 1B and 2B: "Damage from undefended attacks must be dealt to a hero in the
staging area, if able. The first card each player plays each phase does not require a
resource match." With several heroes there, the player whose attack it is chooses which.
Until a player has played a card this phase each hero he controls has every resource
icon for what he plays.
-}
stagingHeroRules :: (HasModifiersM m, Sourceable source) => source -> m ()
stagingHeroRules source = do
  staged <- map toTarget <$> select HeroInStagingArea
  players <- getRemainingPlayers
  effects <- getsGame gameEffects
  for_ players \p -> do
    unless (null staged) $ modified source (PlayerTarget p) (map UndefendedDamageTo staged)
    let played = or [PlayedACardThisPhase `elem` effectModifiers e | e <- effects, effectTarget e == PlayerTarget p]
    unless played do
      heroes <- select (HeroControlledBy $ PlayerWithId p)
      for_ heroes \h -> modified source h [GainResourceIconFor AnyCard s | s <- [Leadership, Tactics, Spirit, Lore]]
