module LordOfTheRings.Quest.Cards.RohanHasCome (rohanHasCome) where

import LordOfTheRings.Card (CardType (AllyType), cdSphere, toCardCode, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype RohanHasCome = RohanHasCome QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

rohanHasCome :: QuestCard RohanHasCome
rohanHasCome = quest RohanHasCome Cards.rohanHasCome

-- | Progress that would be placed on this stage by questing successfully is instead
-- assigned as damage among enemies in play. (Progress must be placed on the active location
-- first: what the game cancels is what is left past it.)
instance HasModifiersFor RohanHasCome where
  getModifiersFor (RohanHasCome a) = when a.flipped $ modified a GameTarget [QuestProgressCanceledBy (toSource a)]

-- | Forced: At the end of the quest phase, advance to stage 5A.
instance HasAbilities RohanHasCome where
  getAbilities (RohanHasCome a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When QuestPhase | a.flipped]

{- | When Revealed: Set The Witch-king aside, out of play. Each player searches the top 10
cards of his deck for an ally that matches the sphere of a hero he controls and puts it
into play under his control. Each player shuffles his deck.
-}
instance RunMessage RohanHasCome where
  runMessage msg q@(RohanHasCome attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      RohanHasCome <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      select (EnemyWithCardCode $ toCardCode Enemies.theWitchKing) >>= traverse_ setAsideEnemy
      eachPlayer \p -> do
        heroes <- traverse getHero =<< select (HeroControlledBy $ PlayerWithId p)
        let spheres = ordNub [s | h <- heroes, Just s <- [cdSphere (toCardDef h)]]
        searchDeckIntoPlay p attrs 10 (CardMatchAll [CardWithType AllyType, CardMatchAny (map CardWithSphere spheres)]) 1 []
      pure q
    ProgressCanceled (isSource attrs -> True) n | n > 0 -> do
      enemies <- select AnyEnemy
      lead <- getLead
      when (notNull enemies) $ dealDamageDividedAmong attrs lead enemies n
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      advanceQuest
      pure q
    _ -> RohanHasCome <$> runMessage msg attrs
