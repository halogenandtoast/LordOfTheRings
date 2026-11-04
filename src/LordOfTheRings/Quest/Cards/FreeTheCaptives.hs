module LordOfTheRings.Quest.Cards.FreeTheCaptives (freeTheCaptives) where

import LordOfTheRings.Card (CardType (EnemyType), toCard)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Trait (Trait (Orc))

newtype FreeTheCaptives = FreeTheCaptives QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

freeTheCaptives :: QuestCard FreeTheCaptives
freeTheCaptives = quest FreeTheCaptives Cards.freeTheCaptives

{- | While an enemy is guarding a hero it gets +1 threat, +1 attack, +1 defense and is
immune to player card effects.
This stage cannot be defeated while an enemy is guarding a hero.
-}
instance HasModifiersFor FreeTheCaptives where
  getModifiersFor (FreeTheCaptives a) = when a.flipped do
    enemies <- filterM (fmap notNull . charactersGuardedBy . (.id) . toCard) =<< getsGame (toList . (.entities.enemies))
    for_ enemies \e -> modified a e [ThreatModifier 1, AttackModifier 1, DefenseModifier 1, ImmuneToPlayerCardEffects]
    unless (null enemies) $ modifySelf a [CannotBeDefeated]

{- | Forced: When the enemy guarding a hero is defeated, that hero's owner takes control of
that hero and exhausts it: exhausted now, it is rescued as its guard leaves play (see
'guardCharacterWith').
-}
instance HasAbilities FreeTheCaptives where
  getAbilities (FreeTheCaptives a) = [mkAbility a 1 $ forced $ MatchingEnemyDefeatedWindow When EnemyGuardingAHero | a.flipped]

{- | When Revealed: Each player searches the encounter deck and discard pile for an Orc
enemy and adds it to the staging area. Shuffle the encounter deck. Attach each hero in the
staging area to a different Orc enemy in the staging area. Each hero attached to an enemy
is guarded by that enemy. Guarded heroes are under no player's control, immune to player
card effects, and their text boxes are considered to be blank.

The first player chooses the Orc each hero goes to; with more heroes than Orcs, those left
stay in the staging area.
-}
instance RunMessage FreeTheCaptives where
  runMessage msg q@(FreeTheCaptives attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      FreeTheCaptives <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> searchEncounterDeckAndDiscard p attrs (CardWithType EnemyType <> CardWithTrait Orc)
      doStep 1 msg
      pure q
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      heroes <- select HeroInStagingArea
      orcs <- select (EnemyInStagingArea <> EnemyWithTrait Orc)
      lead <- getLead
      guardEach lead heroes orcs
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [eid | Window _ (EnemyDefeated eid) <- ws] \eid -> do
        captor <- getEnemy eid
        heroes <- charactersGuardedBy (toCard captor).id
        for_ [h | h@(HeroTarget _) <- heroes] exhaust
      pure q
    _ -> FreeTheCaptives <$> runMessage msg attrs
   where
    guardEach :: ReverseQueue m => PlayerId -> [HeroId] -> [EnemyId] -> m ()
    guardEach lead (h : hs) orcs@(_ : _) = chooseOrRunOneM lead do
      questionLabeled "freeTheCaptives.guard"
      causedBy attrs
      aimedAt [h]
      targets orcs \o -> do
        guardCharacterWith h =<< getEnemy o
        guardEach lead hs (filter (/= o) orcs)
    guardEach _ _ _ = pure ()
